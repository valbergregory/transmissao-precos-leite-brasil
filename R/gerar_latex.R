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
  s <- gsub(".", ",", formatC(x, format = "f", digits = d), fixed = TRUE)
  s <- sub("^-(0,0*)$", "\\1", s)                         # "-0,000" -> "0,000"
  ifelse(is.na(x), "—", s) }
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
# Artigo 1 da trilogia: integração e simetria/limiar do ajuste. As colunas de Granger
# (liderança) saem do manuscrito e ficam no CSV para o artigo complementar.
p <- read_csv(file.path(RES, "tab_transmissao_par.csv"), show_col_types = FALSE)
sn <- function(x) ifelse(x, "Sim", "Não")
# meia-vida (meses) de um desvio: ln(0,5)/ln(1+α), definida para -1 < α < 0
meia_vida <- function(a) ifelse(!is.na(a) & a < 0 & a > -1, log(0.5) / log(1 + a), NA_real_)
t3 <- p %>% transmute(
  Estado = uf[estado], `$K$` = K_par, `Johansen` = paste0(fnum(johansen_traco, 1), ifelse(coint_johansen, "*", "")),
  `Coint.\\ (P-O)` = sn(coint_phil_oul),
  `$\\beta_{LP}$` = fnum(beta_LP),
  `$\\alpha$` = paste0(fnum(alpha_ajuste), ifelse(cointegrado, "", "\\textsuperscript{\\dag}")), `$p(\\alpha)$` = fp(alpha_pval),
  `Meia-vida (meses)` = fnum(meia_vida(alpha_ajuste), 1))
kbl(t3, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lccccccc",
    caption = "Cointegração, elasticidade de longo prazo e velocidade de ajuste, par a par (estado vs.\\ Média Brasil).",
    label = "par") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = nota(paste0(
    "$K$: defasagens do VAR em nível escolhidas no próprio par (critério de Schwarz). Johansen: estatística do traço para ",
    "H$_0$: $r=0$ (valor crítico a 5\\% = ", fnum(unique(p$johansen_cv5)[1], 2), "); * rejeita. P-O: Phillips-Ouliaris a 5\\%. ",
    "$\\beta_{LP}$: elasticidade de longo prazo (MQO em log). $\\alpha$: coeficiente de correção do ECM; meia-vida ",
    "$=\\ln(0{,}5)/\\ln(1+\\alpha)$. ",
    "\\textsuperscript{\\dag} Nenhum dos testes indica cointegração: o ECM é apenas descritivo.")),
    general_title = "Nota:", footnote_as_chunk = TRUE, escape = FALSE, threeparttable = TRUE) %>%
  salvar("tab_par.tex")

# ---- Tabela 3b: assimetria do ajuste (ECM assimétrico e M-TAR) ----
t3b <- p %>% transmute(
  Estado = paste0(uf[estado], ifelse(cointegrado, "", "\\textsuperscript{\\dag}")),
  `$\\alpha^{+}$` = fnum(alpha_pos), `$\\alpha^{-}$` = fnum(alpha_neg),
  `ECM: $p$ ($\\alpha^{+}=\\alpha^{-}$)` = fp(assimetria_pval), `M-TAR $\\tau=0$: $p$` = fp(mtar_assim_pval),
  `$\\hat\\tau$` = fnum(mtar_tau_chan, 4), `M-TAR $\\hat\\tau$: $p$` = fp(mtar_chan_assim_pval))
kbl(t3b, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lcccccc",
    caption = "Testes de simetria do ajuste, par a par (estado vs.\\ Média Brasil).", label = "assim") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = nota(paste0(
    "$\\alpha^{+}$ ($\\alpha^{-}$): ajuste a desvios positivos (negativos) do equilíbrio no ECM assimétrico; $p$ do teste de Wald ",
    "(Newey-West) para $\\alpha^{+}=\\alpha^{-}$. M-TAR $\\tau=0$: limiar imposto, teste $F$ de $\\rho^{+}=\\rho^{-}$. ",
    "M-TAR $\\hat\\tau$: limiar estimado (Chan, 1993) e $p$-valor por bootstrap de regressores fixos (Hansen, 1996), ",
    "999 réplicas. \\textsuperscript{\\dag} Par sem cointegração.")),
    general_title = "Nota:", footnote_as_chunk = TRUE, escape = FALSE, threeparttable = TRUE) %>%
  salvar("tab_assim.tex")

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

# ---- (FEVD) ----
# A decomposição da variância (tab_fevd.csv), a IRF e a matriz de Granger tratam de liderança e
# propagação de choques e ficam para o artigo complementar; não entram no manuscrito deste artigo.
unlink(file.path(OVL, "tabelas", "tab_fevd.tex"))

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
               "ECM (Engle-Granger)","ECM assimétrico (Wald)","M-TAR (Enders-Siklos)","Hansen-Seo"),
  `Hipótese nula ($H_0$)` = c("série tem raiz unitária","série é estacionária","raiz unitária, sem quebra","posto de cointegração $=r$",
               "não há cointegração","$\\alpha=0$ (sem correção)","$\\alpha^{+}=\\alpha^{-}$ (simétrico)",
               "$\\rho^{+}=\\rho^{-}$ (simétrico)","VECM linear"),
  `Rejeitar $H_0$ indica` = c("série estacionária","série não estacionária","estacionária com uma quebra","há relação(ões) de longo prazo",
               "mercados cointegrados","ajuste ao equilíbrio","transmissão assimétrica",
               "assimetria no ajuste ao limiar","há efeito de limiar (TVECM)"))
kbl(q2, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    align = "lll", caption = "Métodos, hipóteses e interpretação.", label = "metodos") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 9) %>%
  column_spec(1, width = "3.4cm") %>% column_spec(2, width = "5.6cm") %>% column_spec(3, width = "5.6cm") %>%
  row_spec(0, bold = TRUE) %>% salvar_quadro("quadro_metodos.tex")

# ---- Macros com os números citados no texto (overleaf/tabelas/macros.tex) ----
# Todo número da prosa do artigo vem daqui ou das tabelas: ao reestimar, o texto acompanha.
# Nomes de macro só com letras; sufixo = sigla da UF.
sigla <- c("Bahia"="BA","Espirito Santo"="ES","Goias"="GO","Minas Gerais"="MG","Parana"="PR",
           "Rio de Janeiro"="RJ","Santa Catarina"="SC","Sao Paulo"="SP")
info <- readRDS(file.path(RES, "resultados.rds"))$info
mes_ano <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "/", format(d, "%Y"))
txt <- function(x) sub("^-", "$-$", x)                      # sinal de menos no texto corrido
macros <- list()
m <- function(nome, valor) macros[[nome]] <<- as.character(valor)
# amostra e regras de inferência (as duas últimas espelham MTAR_NBOOT e HS_NBOOT do script de análise)
m("NObs", info$n); m("PerIni", mes_ano(info$ini)); m("PerFim", mes_ano(info$fim))
m("NSeries", nrow(r)); m("NEstados", nrow(p)); m("KSistema", info$K)
# constantes de especificação lidas do próprio script de análise (fonte única)
scr <- readLines(file.path("R", "analise_transmissao_leite.R"), encoding = "UTF-8")
const <- function(nome) as.numeric(sub(paste0("^", nome, "\\s*<-\\s*([0-9.]+).*$"), "\\1",
                                       grep(paste0("^", nome, "\\s*<-"), scr, value = TRUE)[1]))
milhar <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
m("NBootMtar", milhar(const("MTAR_NBOOT"))); m("NBootHS", milhar(const("HS_NBOOT")))
m("TrimTV", 100 * const("TVECM_TRIM")); m("TrimMtar", 100 * const("MTAR_TRIM"))
m("NLagECM", const("NLAG_ECM")); m("KMax", const("K_LAG_MAX"))
m("LagTV", sub(".*TVECM\\(Y, lag = ([0-9]+).*", "\\1", grep("TVECM\\(Y, lag =", scr, value = TRUE)[1]))
# lacunas internas preenchidas por interpolação linear
bruto <- read_csv(file.path("dados", "cepea_leite_regioes.csv"), show_col_types = FALSE) %>%
  filter(regiao %in% names(uf)) %>% count(regiao)
lac <- bruto %>% filter(n < info$n)
m("NSerieLacuna", nrow(lac)); m("SerieLacunaUF", lista(unname(uf[lac$regiao])))
m("NMesesLacuna", max(info$n - lac$n))
# descritivas
d_ord <- d %>% arrange(media)
m("PrecoMedMin", fnum(d_ord$media[1], 2)); m("PrecoMedMinUF", d_ord$regiao[1])
m("PrecoMedMax", fnum(tail(d_ord$media, 1), 2)); m("PrecoMedMaxUF", tail(d_ord$regiao, 1))
d_cv <- d %>% arrange(cv_pct)
m("CVMin", fnum(d_cv$cv_pct[1], 1)); m("CVMinUF", d_cv$regiao[1])
m("CVMax", fnum(tail(d_cv$cv_pct, 1), 1)); m("CVMaxUF", tail(d_cv$regiao, 1))
# raiz unitária
adf_t <- r$regiao[r$ADF_nivel < r$ADF_nivel_cv5]; adf_c <- r$regiao[r$ADF_c_nivel < r$ADF_c_cv5]
m("NADFtend", length(adf_t)); m("ADFtendNaoRej", lista(setdiff(r$regiao, adf_t)))
m("NADFc", length(adf_c)); m("ADFcNaoRej", lista(setdiff(r$regiao, adf_c)))
m("NPP", sum(r$PP_nivel < r$ADF_nivel_cv5)); m("NKPSS", sum(r$KPSS_nivel > r$KPSS_cv5))
m("NADFdif", sum(r$ADF_dif < r$ADF_c_cv5))
m("NZA", length(za_rej)); m("ZArej", lista(za_rej))
m("ZAestBA", txt(fnum(r$ZA_nivel[r$regiao == "Bahia"]))); m("ZAcv", txt(cv("ZA_cv5")))
m("ZAquebra", za_moda); m("NZAquebra", max(table(r$ZA_quebra)))
outra <- r %>% filter(paste0(mes_pt[as.integer(substr(ZA_quebra, 6, 7))], "/", substr(ZA_quebra, 1, 4)) != za_moda)
m("ZAoutraUF", lista(outra$regiao))
m("ZAoutra", lista(paste0(mes_pt[as.integer(substr(outra$ZA_quebra, 6, 7))], "/", substr(outra$ZA_quebra, 1, 4))))
# cointegração e ECM
pc <- p %>% filter(cointegrado) %>% mutate(hl = meia_vida(alpha_ajuste), UF = unname(uf[estado]))
m("NCoint", nrow(pc)); m("NCointJoh", sum(p$coint_johansen)); m("NCointPO", sum(p$coint_phil_oul))
m("NaoCoint", lista(unname(uf[p$estado[!p$cointegrado]])))
m("SoJoh", lista(unname(uf[p$estado[p$coint_johansen & !p$coint_phil_oul]])))
m("JohCV", fnum(unique(p$johansen_cv5)[1], 2))
m("JohMin", fnum(min(pc$johansen_traco), 1)); m("JohMax", fnum(max(pc$johansen_traco), 1))
m("JohMaxUF", pc$UF[which.max(pc$johansen_traco)])
m("BetaMin", fnum(min(pc$beta_LP))); m("BetaMinUF", pc$UF[which.min(pc$beta_LP)])
m("BetaMax", fnum(max(pc$beta_LP))); m("BetaMaxUF", pc$UF[which.max(pc$beta_LP)])
BETA_TOL <- 0.05
m("NBetaPerto", sum(abs(pc$beta_LP - 1) <= BETA_TOL)); m("BetaTol", fnum(BETA_TOL, 2))
m("AlphaRapido", txt(fnum(min(pc$alpha_ajuste)))); m("AlphaRapidoUF", pc$UF[which.min(pc$alpha_ajuste)])
m("AlphaLento", txt(fnum(max(pc$alpha_ajuste)))); m("AlphaLentoUF", pc$UF[which.max(pc$alpha_ajuste)])
m("HLMin", fnum(min(pc$hl), 1)); m("HLMax", fnum(max(pc$hl), 1)); m("HLMed", fnum(median(pc$hl), 1))
m("NAlphaSig", sum(pc$alpha_pval < 0.01))
for (i in seq_len(nrow(p))) {
  s2 <- sigla[[p$estado[i]]]
  m(paste0("beta", s2), fnum(p$beta_LP[i])); m(paste0("alpha", s2), txt(fnum(p$alpha_ajuste[i])))
  m(paste0("hl", s2), fnum(meia_vida(p$alpha_ajuste[i]), 1)); m(paste0("joh", s2), fnum(p$johansen_traco[i], 1))
  m(paste0("alphaPos", s2), txt(fnum(p$alpha_pos[i]))); m(paste0("alphaNeg", s2), txt(fnum(p$alpha_neg[i])))
  m(paste0("pAssim", s2), fp(p$assimetria_pval[i])); m(paste0("pMtar", s2), fp(p$mtar_assim_pval[i]))
  m(paste0("pMtarChan", s2), fp(p$mtar_chan_assim_pval[i]))
}
# assimetria
m("NAssimECM", sum(p$assimetria_pval < 0.05)); m("NAssimECMdez", sum(p$assimetria_pval < 0.10))
m("AssimECMdezUF", lista(unname(uf[p$estado[p$assimetria_pval < 0.10]])))
m("NMtar", sum(p$mtar_assim_pval < 0.05)); m("NMtardez", sum(p$mtar_assim_pval < 0.10))
m("MtardezUF", lista(unname(uf[p$estado[p$mtar_assim_pval < 0.10]])))
m("NMtarChan", sum(p$mtar_chan_assim_pval < 0.05)); m("NMtarChandez", sum(p$mtar_chan_assim_pval < 0.10))
m("MtarChanMinP", fp(min(p$mtar_chan_assim_pval)))
m("MtarChanMinPUF", unname(uf[p$estado[which.min(p$mtar_chan_assim_pval)]]))
# TVECM
m("NHS", sum(tv$HS_pval < 0.05)); m("HSrejUF", lista(unname(uf[tv$estado[tv$HS_pval < 0.05]])))
m("HSdezUF", lista(unname(uf[tv$estado[tv$HS_pval >= 0.05 & tv$HS_pval < 0.10]])))
m("NAlphaImpl", sum(tv$alpha_implausivel)); m("AlphaImplUF", lista(unname(uf[tv$estado[tv$alpha_implausivel]])))
m("BetaTVMin", fnum(min(tv$beta_LP_tv))); m("BetaTVMax", fnum(max(tv$beta_LP_tv)))
m("NRegMin", min(c(tv$n_regime1, tv$n_regime2)))
for (i in seq_len(nrow(tv))) {
  s2 <- sigla[[tv$estado[i]]]
  m(paste0("hs", s2), fp(tv$HS_pval[i])); m(paste0("limiar", s2), txt(fnum(tv$limiar[i], 4)))
  m(paste0("tvAum", s2), txt(fnum(tv$ect_regime1[i]))); m(paste0("tvAdois", s2), txt(fnum(tv$ect_regime2[i])))
  m(paste0("nUm", s2), tv$n_regime1[i]); m(paste0("nDois", s2), tv$n_regime2[i])
}
# Johansen do sistema de 9 séries (K do critério de Akaike): posto pelo traço a 5%
js <- readRDS(file.path(RES, "resultados.rds"))$johansen
if (!is.null(js)) {
  rej <- rev(js@teststat > js@cval[, "5pct"])          # ordem r = 0, r <= 1, ...
  m("JohSisRank", if (all(rej)) length(rej) else which(!rej)[1] - 1)
  m("JohSisTendencias", nrow(r) - as.integer(macros$JohSisRank))
}
# pico e vale do preço real (Média Brasil), a partir do painel em log
pn <- read_csv(file.path(RES, "painel_precos.csv"), show_col_types = FALSE)
m("PicoMB", mes_ano(pn$date[which.max(pn[["Media Brasil"]])])); m("ValeMB", mes_ano(pn$date[which.min(pn[["Media Brasil"]])]))
m("PicoMBval", fnum(exp(max(pn[["Media Brasil"]])), 2)); m("ValeMBval", fnum(exp(min(pn[["Media Brasil"]])), 2))
# versões em inglês (ponto decimal, mês por extenso) para o abstract
mes_en <- c("January","February","March","April","May","June","July","August","September","October","November","December")
m("PerIniEn", paste(mes_en[as.integer(format(info$ini, "%m"))], format(info$ini, "%Y")))
m("PerFimEn", paste(mes_en[as.integer(format(info$fim, "%m"))], format(info$fim, "%Y")))
for (nm in c("BetaMin","BetaMax","AlphaLento","AlphaRapido","HLMin","HLMax","hsBA"))
  m(paste0(nm, "En"), gsub(",", ".", macros[[nm]], fixed = TRUE))
stopifnot(all(grepl("^[A-Za-z]+$", names(macros))))
writeLines(c("% Gerado por R/gerar_latex.R a partir de resultados_transmissao/*.csv -- não editar à mão.",
             sprintf("\\newcommand{\\%s}{%s}", names(macros), unlist(macros))),
           file.path(OVL, "tabelas", "macros.tex"))

cat("Tabelas LaTeX geradas em:", file.path(OVL, "tabelas"), "\n")
cat("Figuras copiadas:", length(figs), "\n")
