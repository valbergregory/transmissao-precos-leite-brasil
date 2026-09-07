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
t2 <- r %>% transmute(`Região` = regiao, `ADF nível` = fnum(ADF_nivel),
                      `ADF 1\\textsuperscript{a} dif.` = fnum(ADF_dif),
                      `PP nível` = fnum(PP_nivel), `KPSS nível` = fnum(KPSS_nivel))
kbl(t2, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrrr", caption = "Testes de raiz unitária (níveis em log).", label = "raiz") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>%
  salvar("tab_raiz.tex")

# ---- Tabela 3: transmissão par a par (wide) ----
p <- read_csv(file.path(RES, "tab_transmissao_par.csv"), show_col_types = FALSE)
t3 <- p %>% transmute(
  Estado = uf[estado], `Coint. (P-O)` = ifelse(coint_phil_oul, "Sim", "Não"),
  `$\\beta_{LP}$` = fnum(beta_LP), `$\\alpha$` = fnum(alpha_ajuste), `$p(\\alpha)$` = fp(alpha_pval),
  `$\\alpha^{+}$` = fnum(alpha_pos), `$\\alpha^{-}$` = fnum(alpha_neg),
  `Assim. ECM` = fp(assimetria_pval), `Assim. M-TAR` = fp(mtar_assim_pval),
  `Granger ref$\\to$est` = fp(granger_ref2est), `Granger est$\\to$ref` = fp(granger_est2ref))
kbl(t3, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lcccccccccc",
    caption = "Transmissão de preços, par a par (estado vs.\\ Média Brasil).", label = "par") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  row_spec(0, bold = TRUE) %>%
  salvar("tab_par.tex")

# ---- Tabela 4: TVECM ----
tv <- read_csv(file.path(RES, "tab_tvecm.csv"), show_col_types = FALSE)
t4 <- tv %>% transmute(
  Estado = uf[estado], Limiar = fnum(limiar, 4), `$\\beta_{LP}$` = fnum(beta_LP_tv),
  `$\\alpha$ reg.\\ 1` = fnum(ect_regime1), `$\\alpha$ reg.\\ 2` = fnum(ect_regime2),
  `$n_1$` = n_regime1, `$n_2$` = n_regime2, `Hansen-Seo ($p$)` = fp(HS_pval))
kbl(t4, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lrrrrrrr",
    caption = "VECM com limiar (TVECM): estado vs.\\ Média Brasil. Regime~1 = desvios abaixo do limiar; regime~2 = acima.",
    label = "tvecm") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9) %>%
  row_spec(0, bold = TRUE) %>%
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
                  "IPCA --- número-índice (IBGE, via API do IPEA); preços reais",
                  "Logaritmo natural (coeficientes $\\approx$ elasticidades)"))
kbl(q1, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "ll", caption = "Fonte e variáveis.", label = "fonte") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  column_spec(1, bold = TRUE, width = "4cm") %>% column_spec(2, width = "11cm") %>%
  salvar_quadro("quadro_fonte.tex")

# ---- Quadro 2: métodos e hipóteses ----
q2 <- tibble(
  `Método` = c("ADF / Phillips-Perron","KPSS","Johansen (traço)","Phillips-Ouliaris",
               "ECM assimétrico (Wald)","M-TAR (Enders-Siklos)","Hansen-Seo","Granger"),
  `Hipótese nula ($H_0$)` = c("série tem raiz unitária","série é estacionária","posto de cointegração $=r$",
               "não há cointegração","$\\alpha^{+}=\\alpha^{-}$ (simétrico)","$\\rho^{+}=\\rho^{-}$ (simétrico)",
               "VECM linear","$X$ não antecede $Y$"),
  `Rejeitar $H_0$ indica` = c("série estacionária","série não estacionária","há relação(ões) de longo prazo",
               "mercados cointegrados","transmissão assimétrica","assimetria no ajuste ao limiar",
               "há efeito de limiar (TVECM)","$X$ ajuda a prever $Y$"))
kbl(q2, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lll", caption = "Métodos, hipóteses e interpretação.", label = "metodos") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9) %>%
  column_spec(1, width = "3.4cm") %>% column_spec(2, width = "5.6cm") %>% column_spec(3, width = "6cm") %>%
  row_spec(0, bold = TRUE) %>% salvar_quadro("quadro_metodos.tex")

cat("Tabelas LaTeX geradas em:", file.path(OVL, "tabelas"), "\n")
cat("Figuras copiadas:", length(figs), "\n")
