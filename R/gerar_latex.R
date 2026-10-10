# Gera as tabelas LaTeX (booktabs via kableExtra) e a estrutura do projeto Overleaf
suppressPackageStartupMessages({library(readr); library(dplyr); library(kableExtra)})

# Caminhos relativos à raiz do projeto (rode a partir da raiz do repositório)
RES <- "resultados_transmissao"
FIG <- file.path(RES, "fig")
OVL <- "overleaf"
dir.create(file.path(OVL, "tabelas"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OVL, "figuras"), recursive = TRUE, showWarnings = FALSE)

# copia figuras
figs <- list.files(FIG, pattern = "\\.png$", full.names = TRUE)
file.copy(figs, file.path(OVL, "figuras", basename(figs)), overwrite = TRUE)

uf <- c("Bahia"="Bahia","Espirito Santo"="Espírito Santo","Goias"="Goiás",
        "Minas Gerais"="Minas Gerais","Parana"="Paraná","Rio de Janeiro"="Rio de Janeiro",
        "Santa Catarina"="Santa Catarina","Sao Paulo"="São Paulo","Media Brasil"="Média Brasil")

fnum <- function(x, d = 3) { x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", gsub(".", ",", formatC(x, format = "f", digits = d), fixed = TRUE)) }
fp <- function(x) { x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", ifelse(x < 0.001, "$<$0,001",
    gsub(".", ",", formatC(x, format = "f", digits = 3), fixed = TRUE))) }
fpct <- function(x) paste0(gsub(".", ",", formatC(100 * as.numeric(x), format = "f", digits = 1), fixed = TRUE), "\\%")

# footnote() do kableExtra consome um nível de barras invertidas mesmo com escape = FALSE
nota <- function(x) gsub("\\", "\\\\", x, fixed = TRUE)
salvar <- function(kobj, arquivo) writeLines(as.character(kobj), file.path(OVL, "tabelas", arquivo))
# Quadros: converte o ambiente table -> quadro (contador próprio) e tab:->quad:
salvar_quadro <- function(kobj, arquivo) {
  s <- as.character(kobj)
  s <- gsub("\\begin{table}", "\\begin{quadro}", s, fixed = TRUE)
  s <- gsub("\\end{table}",   "\\end{quadro}",   s, fixed = TRUE)
  s <- gsub("tab:fonte", "quad:fonte", s, fixed = TRUE)
  s <- gsub("tab:metodos", "quad:metodos", s, fixed = TRUE)
  writeLines(s, file.path(OVL, "tabelas", arquivo))
}

# ---- Tabela 1: descritivas ----
d <- read_csv(file.path(RES, "tab_descritiva.csv"), show_col_types = FALSE)
t1 <- d %>% transmute(`Região` = regiao, `Média` = fnum(media, 3), `Desv.-padrão` = fnum(desv_pad, 3),
                      `Mínimo` = fnum(minimo, 3), `Máximo` = fnum(maximo, 3), `CV (\\%)` = fnum(cv_pct, 1))
kbl(t1, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrrrr", caption = "Estatísticas descritivas do preço real do leite (R\\$/litro).",
    label = "descritiva") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>% salvar("tab_descritiva.tex")

# ---- Tabela 2: raiz unitária ----
r <- read_csv(file.path(RES, "tab_raiz_unitaria.csv"), show_col_types = FALSE)
cv <- function(col) fnum(unique(na.omit(r[[col]]))[1])
lista <- function(x) if (length(x) == 0) "nenhuma série" else if (length(x) == 1) x else paste(paste(head(x, -1), collapse = ", "), "e", tail(x, 1))
za_rej <- r$regiao[!is.na(r$ZA_nivel) & r$ZA_nivel < r$ZA_cv5]
mes_pt <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
za_moda <- names(sort(table(r$ZA_quebra), decreasing = TRUE))[1]
za_moda <- paste0(mes_pt[as.integer(substr(za_moda, 6, 7))], "/", substr(za_moda, 1, 4))
t2 <- r %>% transmute(`Região` = regiao, `ADF (tend.)` = fnum(ADF_nivel), `ADF (interc.)` = fnum(ADF_c_nivel),
                      `PP` = fnum(PP_nivel), `KPSS` = fnum(KPSS_nivel),
                      `ZA` = fnum(ZA_nivel), `Quebra ZA` = ZA_quebra,
                      `ADF 1\\textsuperscript{a} dif.` = fnum(ADF_dif))
kbl(t2, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrrrrcr", caption = "Testes de raiz unitária (níveis em log do preço real).", label = "raiz") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = nota(paste0(
    "Valores críticos a 5\\%: ADF com tendência ", cv("ADF_nivel_cv5"), "; ADF com intercepto ", cv("ADF_c_cv5"),
    "; KPSS (H$_0$: estacionária em torno de tendência) ", cv("KPSS_cv5"), "; Zivot-Andrews (quebra em nível e tendência) ", cv("ZA_cv5"),
    ". O ADF com tendência rejeita a raiz unitária na maioria das séries, mas o KPSS rejeita a estacionariedade em todas e o ",
    "Zivot-Andrews, que admite uma quebra endógena, só a rejeita em: ", lista(za_rej), ". Tratamos as séries como I(1); a quebra estimada ",
    "concentra-se em ", za_moda, ".")),
    general_title = "Nota:", footnote_as_chunk = TRUE, escape = FALSE, threeparttable = TRUE) %>%
  salvar("tab_raiz.tex")

# ---- Tabela 3: transmissão par a par (wide) ----
p <- read_csv(file.path(RES, "tab_transmissao_par.csv"), show_col_types = FALSE)
sn <- function(x) ifelse(x, "Sim", "Não")
t3 <- p %>% transmute(
  Estado = uf[estado], `$K$` = K_par, `Johansen` = paste0(fnum(johansen_traco, 1), ifelse(coint_johansen, "*", "")),
  `Coint. (P-O)` = sn(coint_phil_oul),
  `$\\beta_{LP}$` = fnum(beta_LP),
  `$\\alpha$` = paste0(fnum(alpha_ajuste), ifelse(cointegrado, "", "\\textsuperscript{\\dag}")), `$p(\\alpha)$` = fp(alpha_pval),
  `$\\alpha^{+}$` = fnum(alpha_pos), `$\\alpha^{-}$` = fnum(alpha_neg),
  `Assim. ECM` = fp(assimetria_pval), `M-TAR $\\tau=0$` = fp(mtar_assim_pval),
  `$\\hat\\tau$` = fnum(mtar_tau_chan, 4), `M-TAR $\\hat\\tau$` = fp(mtar_chan_assim_pval),
  `Granger ref$\\to$est` = fp(granger_ref2est), `Granger est$\\to$ref` = fp(granger_est2ref))
kbl(t3, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lcccccccccccccc",
    caption = "Transmissão de preços, par a par (estado vs.\\ Média Brasil).", label = "par") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = nota(paste0(
    "$K$: defasagens do VAR em nível escolhidas no próprio par (critério de Schwarz). Johansen: estatística do traço para ",
    "H$_0$: $r=0$ (valor crítico a 5\\% = ", fnum(unique(p$johansen_cv5)[1], 2), "); * rejeita. ",
    "\\textsuperscript{\\dag} Nenhum dos testes indica cointegração: o ECM é apenas descritivo. ",
    "M-TAR $\\tau=0$: limiar imposto, teste $F$ de simetria. M-TAR $\\hat\\tau$: limiar estimado (Chan, 1993) e $p$-valor por ",
    "bootstrap de regressores fixos (Hansen, 1996), 999 réplicas.")),
    general_title = "Nota:", footnote_as_chunk = TRUE, escape = FALSE, threeparttable = TRUE) %>%
  salvar("tab_par.tex")

# ---- Tabela 4: TVECM ----
tv <- read_csv(file.path(RES, "tab_tvecm.csv"), show_col_types = FALSE)
marca <- function(a) paste0(fnum(a), ifelse(abs(a) > 1, "\\textsuperscript{\\ddag}", ""))
t4 <- tv %>% transmute(
  Estado = uf[estado], Limiar = fnum(limiar, 4), `$\\beta_{LP}$` = fnum(beta_LP_tv),
  `$\\alpha$ reg.\\ 1` = marca(ect_regime1), `$\\alpha$ reg.\\ 2` = marca(ect_regime2),
  `$n_1$` = n_regime1, `$n_2$` = n_regime2, `Hansen-Seo ($p$)` = fp(HS_pval))
kbl(t4, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrrrrrr",
    caption = "VECM com limiar (TVECM): estado vs.\\ Média Brasil. Regime~1 = desvios abaixo do limiar; regime~2 = acima.",
    label = "tvecm") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = nota(paste0(
    "$\\alpha$: coeficiente do termo de correção na equação do estado. Hansen-Seo com 1.000 réplicas de bootstrap e ",
    "15\\% mínimo de observações por regime. \\textsuperscript{\\ddag} $|\\alpha|>1$ implica corrigir mais que o desvio inteiro ",
    "em um mês: sobreajuste em regime com poucas observações; não interpretar. Rejeitam a linearidade a 5\\%: ",
    lista(unname(uf[tv$estado[!is.na(tv$HS_pval) & tv$HS_pval < 0.05]])), ".")),
    general_title = "Nota:", footnote_as_chunk = TRUE, escape = FALSE, threeparttable = TRUE) %>%
  salvar("tab_tvecm.tex")

# ---- Tabela 5: FEVD ----
fv <- read_csv(file.path(RES, "tab_fevd.csv"), show_col_types = FALSE)
t5 <- fv %>% transmute(Estado = uf[estado], `\\% Referência` = fpct(share_ref),
                       `\\% Própria` = fpct(share_propria), `\\% Outros` = fpct(share_outros))
kbl(t5, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrr",
    caption = "Decomposição da variância do erro de previsão a 12 meses.", label = "fevd") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>% salvar("tab_fevd.tex")

# ---- Quadro 1: fonte e variáveis ----
q1 <- tibble(
  Item = c("Fonte","Variável","Frequência / Período","Regiões","Deflator","Transformação"),
  `Descrição` = c("CEPEA/ESALQ-USP --- Indicador do preço do leite ao produtor",
                  "Preço líquido médio recebido pelo produtor (R\\$/litro)",
                  "Mensal --- jan/2016 a abr/2026 (124 observações)",
                  "Média Brasil + 8 estados (BA, ES, GO, MG, PR, RJ, SC, SP)",
                  "IPCA --- número-índice (IBGE), em dados/ipca\\_indice.csv; preços reais de abr/2026",
                  "Logaritmo natural (coeficientes $\\approx$ elasticidades)"))
kbl(q1, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "ll", caption = "Fonte e variáveis.", label = "fonte") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "4cm") %>% column_spec(2, width = "11cm") %>%
  salvar_quadro("quadro_fonte.tex")

# ---- Quadro 2: métodos e hipóteses ----
q2 <- tibble(
  `Método` = c("ADF / Phillips-Perron","KPSS","Zivot-Andrews","Johansen (traço)","Phillips-Ouliaris",
               "ECM assimétrico (Wald)","M-TAR (Enders-Siklos)","Hansen-Seo","Granger"),
  `Hipótese nula ($H_0$)` = c("série tem raiz unitária","série é estacionária","raiz unitária, sem quebra","posto de cointegração $=r$",
               "não há cointegração","$\\alpha^{+}=\\alpha^{-}$ (simétrico)","$\\rho^{+}=\\rho^{-}$ (simétrico)",
               "VECM linear","$X$ não antecede $Y$"),
  `Rejeitar $H_0$ indica` = c("série estacionária","série não estacionária","estacionária com uma quebra","há relação(ões) de longo prazo",
               "mercados cointegrados","transmissão assimétrica","assimetria no ajuste ao limiar",
               "há efeito de limiar (TVECM)","$X$ ajuda a prever $Y$"))
kbl(q2, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lll", caption = "Métodos, hipóteses e interpretação.", label = "metodos") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9) %>%
  column_spec(1, width = "3.4cm") %>% column_spec(2, width = "5.6cm") %>% column_spec(3, width = "6cm") %>%
  row_spec(0, bold = TRUE) %>% salvar_quadro("quadro_metodos.tex")

cat("Tabelas LaTeX geradas em:", file.path(OVL, "tabelas"), "\n")
cat("Figuras copiadas:", length(figs), "\n")
