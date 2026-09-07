# =============================================================================
# TRANSMISSÃO DE PREÇOS DO LEITE ENTRE REGIÕES BRASILEIRAS — CEPEA/ESALQ-USP
# =============================================================================
# Este script estima, de forma reprodutível, a INTEGRAÇÃO e a TRANSMISSÃO de
# preços do leite ao produtor entre regiões do Brasil. Cada bloco abaixo traz,
# em comentário, (i) O QUE é feito, (ii) POR QUE (fundamento econométrico) e
# (iii) COMO INTERPRETAR o resultado. Toda figura/tabela é exportada para a
# pasta de resultados, pronta para entrar no artigo.
#
# Dados: preço líquido médio recebido pelo produtor (R$/litro), mensal,
#        jan/2016–abr/2026, por região (CEPEA/ESALQ-USP), deflacionado pelo
#        IPCA (IBGE, via API do IPEA). Apenas regiões com cobertura completa.
#
# Roteiro metodológico (literatura clássica de transmissão de preços):
#   1. Raiz unitária (Dickey-Fuller Aumentado, KPSS, Phillips-Perron)
#        -> estabelece a ordem de integração; preços costumam ser I(1).
#   2. Cointegração de Johansen (1988, 1991) e Phillips-Ouliaris (1990)
#        -> existe relação de equilíbrio de longo prazo entre os mercados?
#   3. Modelo de Correção de Erro (ECM; Engle & Granger, 1987)
#        -> velocidade com que cada mercado retorna ao equilíbrio.
#   4. ECM ASSIMÉTRICO (von Cramon-Taubadel & Loy, 1996) e cointegração com
#      limiar M-TAR (Enders & Siklos, 2001)
#        -> a transmissão é simétrica a altas e baixas de preço?
#   5. TVECM — VECM com LIMIAR (Hansen & Seo, 2002; Balke & Fox, 1997)
#        -> banda de não-arbitragem: dentro da banda (custos de transação) o
#           ajuste é fraco/nulo; fora dela, a arbitragem corrige o desvio.
#   6. Causalidade de Granger (1969) e VAR/IRF/FEVD (Sims, 1980)
#        -> liderança no descobrimento de preços e propagação de choques.
#
# Pré-requisito: rode antes 'construir_base_cepea.R' (ou tenha o arquivo
#   dados/cepea_leite_regioes.csv gerado a partir dos .xls do CEPEA).
# =============================================================================

# 0. Pacotes ------------------------------------------------------------------
required_pkgs <- c(
  "tidyverse", "lubridate", "zoo", "urca", "vars", "tseries", "forecast",
  "lmtest", "sandwich", "tsDyn", "reshape2", "scales", "ggplot2", "plotly",
  "htmlwidgets", "httr", "jsonlite"
)
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando pacote: ", pkg); install.packages(pkg, dependencies = TRUE)
  }
}
invisible(lapply(required_pkgs, install_if_missing))
suppressPackageStartupMessages(invisible(lapply(required_pkgs, library, character.only = TRUE)))

theme_set(theme_minimal(base_size = 13))
set.seed(123)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
safe <- function(expr) tryCatch(expr, error = function(e) {
  message("[aviso] ", conditionMessage(e)); NULL })

# 1. Parâmetros ---------------------------------------------------------------
ARQ_CEPEA   <- file.path("dados", "cepea_leite_regioes.csv")
COL_PRECO   <- "liq_med"             # preço líquido médio (série contínua)
REFERENCIA  <- "Media Brasil"        # mercado de referência p/ análise par a par
PAR_ILUSTRA <- "Minas Gerais"        # par destacado nas figuras (maior bacia leiteira)
DEFLACIONAR <- TRUE                  # TRUE = preços reais (IPCA); FALSE = nominais
USAR_LOG    <- TRUE                  # logaritmo (coef. ~ elasticidades)
K_LAG_MAX   <- 8                     # nº máx. de defasagens p/ seleção
NLAG_ECM    <- 2                     # defasagens nos modelos de curto prazo
TVECM_TRIM  <- 0.15                  # fração mín. de obs por regime (TVECM)
HS_NBOOT    <- 100                   # reamostragens do teste de Hansen-Seo

REGIOES <- c("Media Brasil", "Bahia", "Espirito Santo", "Goias",
             "Minas Gerais", "Parana", "Rio de Janeiro",
             "Santa Catarina", "Sao Paulo")
ESTADOS <- setdiff(REGIOES, REFERENCIA)

rotulo <- c("Media Brasil" = "Média Brasil", "Bahia" = "Bahia",
            "Espirito Santo" = "Espírito Santo", "Goias" = "Goiás",
            "Minas Gerais" = "Minas Gerais", "Parana" = "Paraná",
            "Rio de Janeiro" = "Rio de Janeiro", "Santa Catarina" = "Santa Catarina",
            "Sao Paulo" = "São Paulo")

DIR_OUT <- "resultados_transmissao"
DIR_FIG <- file.path(DIR_OUT, "fig")
for (d in c(DIR_OUT, DIR_FIG)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
out_path <- function(f) file.path(DIR_OUT, f)
fig_path <- function(f) file.path(DIR_FIG, f)

salvar_widget <- function(widget, arquivo) {
  ok <- tryCatch({ htmlwidgets::saveWidget(widget, out_path(arquivo), selfcontained = TRUE); TRUE },
                 error = function(e) tryCatch({
                   htmlwidgets::saveWidget(widget, out_path(arquivo), selfcontained = FALSE); TRUE
                 }, error = function(e2) { message("[aviso] ", arquivo, ": ", conditionMessage(e2)); FALSE }))
  invisible(ok)
}
# salva figura estática em alta resolução (para inserir no documento)
salvar_fig <- function(plot, arquivo, w = 9, h = 5.2) {
  ggplot2::ggsave(fig_path(arquivo), plot, width = w, height = h, dpi = 300, bg = "white")
  message("Figura salva: ", fig_path(arquivo))
}

# 2. Deflator IPCA via API IPEA -----------------------------------------------
# POR QUÊ: para comparar mercados ao longo de 10 anos é preciso remover a
# inflação. Usamos o número-índice do IPCA (IBGE) e expressamos tudo a preços
# do último mês (preço real). Assim os movimentos refletem variação relativa,
# não inflação geral.
obter_ipca_indice <- function() {
  cache_dir <- "cache_ipea"; if (!dir.exists(cache_dir)) dir.create(cache_dir)
  cf <- file.path(cache_dir, "ipca_indice.rds")
  if (file.exists(cf) && difftime(Sys.time(), file.info(cf)$mtime, units = "days") < 7)
    return(readRDS(cf))
  url <- "http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_IPCA12')"
  resp <- httr::RETRY("GET", url, httr::timeout(120), times = 4, quiet = TRUE)
  if (httr::http_error(resp)) stop("Falha ao obter IPCA na API do IPEA.")
  val <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)$value
  ipca <- tibble(date = as.Date(substr(val$VALDATA, 1, 10)),
                 indice = as.numeric(val$VALVALOR)) %>%
    filter(!is.na(date), !is.na(indice)) %>% arrange(date)
  saveRDS(ipca, cf); ipca
}

# 3. Carregamento e construção do painel balanceado ---------------------------
# O QUE: lê o CSV tidy, mantém as regiões de cobertura completa, deflaciona e
# organiza um painel mensal balanceado (matriz tempo x região), em log.
if (!file.exists(ARQ_CEPEA)) stop("Arquivo não encontrado: ", ARQ_CEPEA,
                                  "\nGere-o antes com construir_base_cepea.R.")
bruto <- readr::read_csv(ARQ_CEPEA, show_col_types = FALSE) %>%
  filter(regiao %in% REGIOES) %>%
  mutate(date = lubridate::floor_date(as.Date(date), "month")) %>%
  select(regiao, date, preco = all_of(COL_PRECO))

if (DEFLACIONAR) {
  ipca <- safe(obter_ipca_indice())
  if (!is.null(ipca)) {
    base_idx <- ipca %>% filter(date == max(bruto$date)) %>% pull(indice)
    if (length(base_idx) == 0) base_idx <- tail(ipca$indice, 1)
    bruto <- bruto %>% left_join(ipca, by = "date") %>%
      mutate(preco = preco * base_idx / indice) %>% select(-indice)
    message("Preços deflacionados pelo IPCA (base = ", format(max(bruto$date)), ").")
  } else { message("[aviso] IPCA indisponível; usando preços NOMINAIS."); DEFLACIONAR <- FALSE }
}

grade <- seq(min(bruto$date), max(bruto$date), by = "month")
wide <- bruto %>%
  tidyr::pivot_wider(names_from = regiao, values_from = preco) %>%
  tidyr::complete(date = grade) %>% arrange(date)
# interpola buracos internos (BA/ES/RJ têm 1 mês faltante) -> painel balanceado
for (r in REGIOES) wide[[r]] <- zoo::na.approx(wide[[r]], na.rm = FALSE)
wide <- tidyr::drop_na(wide)
precos_nivel <- wide                                  # guarda nível (R$) p/ stats
if (USAR_LOG) wide <- wide %>% mutate(across(all_of(REGIOES), log))

message(sprintf("Painel: %d meses (%s a %s), %d regiões.",
                nrow(wide), format(min(wide$date)), format(max(wide$date)), length(REGIOES)))

freq_ini  <- c(year(min(wide$date)), month(min(wide$date)))
mk_ts     <- function(col) ts(wide[[col]], start = freq_ini, frequency = 12)
series_ts <- lapply(REGIOES, mk_ts); names(series_ts) <- REGIOES   # já em log (real)
M <- as.matrix(wide[, REGIOES]); colnames(M) <- REGIOES

# 3b. Estatísticas descritivas (Tabela 1) -------------------------------------
tab_desc <- precos_nivel %>%
  pivot_longer(-date, names_to = "regiao", values_to = "p") %>%
  group_by(regiao) %>%
  summarise(media = mean(p), desv_pad = sd(p), minimo = min(p),
            maximo = max(p), cv_pct = 100 * sd(p) / mean(p), .groups = "drop") %>%
  mutate(regiao = rotulo[regiao]) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
readr::write_csv(tab_desc, out_path("tab_descritiva.csv"))

# 4. Figuras descritivas -------------------------------------------------------
dlong <- wide %>% pivot_longer(-date, names_to = "regiao", values_to = "preco") %>%
  mutate(regiao = factor(rotulo[regiao], levels = unname(rotulo[REGIOES])))
g_series <- ggplot(dlong, aes(date, preco, color = regiao)) +
  geom_line(linewidth = 0.7) +
  labs(title = paste0(ifelse(USAR_LOG, "Log do ", ""),
                      ifelse(DEFLACIONAR, "preço real", "preço nominal"),
                      " do leite ao produtor (CEPEA)"),
       subtitle = "Mercados regionais, mensal", x = NULL, y = NULL, color = NULL)
salvar_fig(g_series, "fig01_series.png"); salvar_widget(ggplotly(g_series, dynamicTicks = TRUE), "01_series_regioes.html")

# Diferencial (spread) de cada estado vs. referência: se for estacionário em
# torno de um nível, é um forte indício visual de cointegração / Lei do Preço Único.
g_spread <- wide %>%
  mutate(across(all_of(ESTADOS), ~ .x - .data[[REFERENCIA]])) %>%
  select(date, all_of(ESTADOS)) %>%
  pivot_longer(-date, names_to = "regiao", values_to = "spread") %>%
  mutate(regiao = rotulo[regiao]) %>%
  ggplot(aes(date, spread, color = regiao)) + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = paste0("Diferencial de preço (log) vs. ", rotulo[REFERENCIA]),
       x = NULL, y = "Diferença (log)", color = NULL)
salvar_fig(g_spread, "fig02_spreads.png"); salvar_widget(ggplotly(g_spread), "02_spreads.html")

# Co-movimento contemporâneo: correlação das variações mensais (retornos log).
corr_ret <- cor(diff(M))
g_heat <- reshape2::melt(corr_ret) %>%
  mutate(Var1 = rotulo[as.character(Var1)], Var2 = rotulo[as.character(Var2)]) %>%
  ggplot(aes(Var1, Var2, fill = value)) + geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       midpoint = 0.5, limits = c(-1, 1), name = "corr") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlação das variações mensais de preço", x = NULL, y = NULL)
salvar_fig(g_heat, "fig03_corr_heatmap.png", w = 8, h = 6.5)

# 5. Testes de raiz unitária (Tabela 2) ---------------------------------------
# O QUE: para cada série testamos H0 = "tem raiz unitária" (não estacionária).
# ADF e PP: rejeitar H0 (|estatística| > |valor crítico|) => estacionária.
# KPSS: H0 = estacionária; rejeitar (estat. > crítico) => NÃO estacionária.
# ESPERADO: nível I(1) (não rejeita ADF/PP; rejeita KPSS) e 1ª diferença I(0).
testar_raiz <- function(x, nome) {
  adf_l <- safe(ur.df(x, type = "trend", selectlags = "AIC"))
  adf_d <- safe(ur.df(diff(x), type = "drift", selectlags = "AIC"))
  kp    <- safe(ur.kpss(x, type = "tau"))
  pp    <- safe(ur.pp(x, type = "Z-tau", model = "trend", lags = "short"))
  tibble(
    regiao        = rotulo[nome],
    ADF_nivel     = if (!is.null(adf_l)) round(adf_l@teststat["statistic", "tau3"], 3) else NA,
    ADF_nivel_cv5 = if (!is.null(adf_l)) round(adf_l@cval["tau3", "5pct"], 3) else NA,
    ADF_dif       = if (!is.null(adf_d)) round(adf_d@teststat["statistic", "tau2"], 3) else NA,
    PP_nivel      = if (!is.null(pp)) round(pp@teststat[1], 3) else NA,
    KPSS_nivel    = if (!is.null(kp)) round(kp@teststat[1], 3) else NA,
    KPSS_cv5      = if (!is.null(kp)) round(kp@cval[1, "5pct"], 3) else NA
  )
}
tab_raiz <- map_dfr(REGIOES, ~ testar_raiz(series_ts[[.x]], .x))
cat("\n=== Tabela 2: Raiz unitária ===\n"); print(as.data.frame(tab_raiz), row.names = FALSE)
readr::write_csv(tab_raiz, out_path("tab_raiz_unitaria.csv"))

# 6. Cointegração de Johansen — sistema completo ------------------------------
# O QUE: testa quantas relações de equilíbrio de longo prazo (posto r) existem
# entre as 9 séries. Traço: começa em H0: r=0; se rejeita, passa a r<=1, etc.
# O primeiro r não rejeitado é o posto estimado. r>=1 => mercados cointegrados.
lag_sel <- safe(VARselect(M, lag.max = K_LAG_MAX, type = "const")$selection["AIC(n)"])
K <- max(2, min(K_LAG_MAX, lag_sel %||% 2))
joh_sys <- safe(ca.jo(M, type = "trace", ecdet = "const", K = K, spec = "transitory"))
cat("\n=== Johansen (traço) — sistema,", length(REGIOES), "séries, K =", K, "===\n")
if (!is.null(joh_sys)) print(summary(joh_sys))

# 7. Análise PAR A PAR: cada estado vs. referência ----------------------------
# Modelos estimados para o par (y = estado, x = referência):
#   Longo prazo:   y_t = b0 + b1 x_t + u_t        (b1 = elasticidade de LP)
#   ECM simétrico: Δy_t = c + α u_{t-1} + lags(Δy,Δx) + ε
#       α<0 e significativo => correção ao equilíbrio; |α| = velocidade/mês.
#   ECM assimétrico: separa u_{t-1} em parte positiva (preço acima do equilíbrio)
#       e negativa; H0: α+ = α- (Wald). Rejeitar => transmissão ASSIMÉTRICA
#       (ex.: repasse mais rápido de altas que de baixas — "rockets & feathers").
#   M-TAR (Enders-Siklos): limiar no AJUSTE do resíduo; H0: ρ+ = ρ- (simetria).
#   Granger: testa precedência temporal (quem "move primeiro").
car_like_wald <- function(modelo) {            # Wald p/ α+ = α- (cov. HAC Newey-West)
  b  <- coef(modelo); V <- sandwich::NeweyWest(modelo)
  R  <- numeric(length(b)); names(R) <- names(b)
  R["ect_pos"] <- 1; R["ect_neg"] <- -1
  W <- (as.numeric(R %*% b))^2 / as.numeric(t(R) %*% V %*% R)
  pchisq(W, df = 1, lower.tail = FALSE)
}
linearHypothesis_F <- function(modelo, a, b) { # teste F p/ igualdade de 2 coefs
  d <- modelo$model; y <- model.response(d); X <- model.matrix(modelo)
  Xr <- X; Xr[, a] <- Xr[, a] + Xr[, b]; Xr <- Xr[, setdiff(colnames(Xr), b), drop = FALSE]
  rss_f <- sum(residuals(modelo)^2); rss_r <- sum(residuals(lm.fit(Xr, y))^2)
  Fst <- ((rss_r - rss_f)) / (rss_f / modelo$df.residual)
  pf(Fst, 1, modelo$df.residual, lower.tail = FALSE)
}
analisar_par <- function(estado, ref, nlag = NLAG_ECM) {
  y <- as.numeric(series_ts[[estado]]); x <- as.numeric(series_ts[[ref]])

  jo <- safe(ca.jo(cbind(y, x), type = "trace", ecdet = "const", K = max(2, K), spec = "transitory"))
  coint_joh <- if (!is.null(jo)) jo@teststat[length(jo@teststat)] > jo@cval[nrow(jo@cval), "5pct"] else NA
  po <- safe(ca.po(cbind(y, x), demean = "constant", type = "Pu"))
  coint_po <- if (!is.null(po)) as.logical(po@teststat[1] > po@cval[1, "5pct"]) else NA

  lr <- lm(y ~ x); u <- residuals(lr)
  dy <- diff(y); dx <- diff(x); ect <- head(u, -1)
  df <- tibble(dy = dy, dx = dx, ect = ect)
  for (i in seq_len(nlag)) { df[[paste0("dy", i)]] <- dplyr::lag(dy, i); df[[paste0("dx", i)]] <- dplyr::lag(dx, i) }
  df <- tidyr::drop_na(df)
  lt <- paste(c(paste0("dy", seq_len(nlag)), paste0("dx", seq_len(nlag)), "dx"), collapse = " + ")
  ecm_sym <- lm(as.formula(paste("dy ~ ect +", lt)), data = df)
  alpha <- coef(ecm_sym)["ect"]; alpha_p <- summary(ecm_sym)$coefficients["ect", 4]

  df2 <- df %>% mutate(ect_pos = pmax(ect, 0), ect_neg = pmin(ect, 0))
  ecm_asy <- lm(as.formula(paste("dy ~ ect_pos + ect_neg +", lt)), data = df2)
  a_pos <- coef(ecm_asy)["ect_pos"]; a_neg <- coef(ecm_asy)["ect_neg"]
  assim_p <- safe(car_like_wald(ecm_asy))

  du <- diff(u); ut1 <- head(u, -1); dut1 <- c(NA, head(du, -1)); Ih <- as.numeric(dut1 >= 0)
  mdf <- tibble(du = du, pos = Ih * ut1, neg = (1 - Ih) * ut1)
  for (i in seq_len(nlag)) mdf[[paste0("du", i)]] <- dplyr::lag(du, i)
  mdf <- tidyr::drop_na(mdf)
  mtar <- lm(as.formula(paste("du ~ 0 + pos + neg +", paste0("du", seq_len(nlag), collapse = " + "))), data = mdf)
  mtar_p <- safe(linearHypothesis_F(mtar, "pos", "neg"))

  gx2y <- safe(lmtest::grangertest(dy ~ dx, order = nlag))   # ref -> estado
  gy2x <- safe(lmtest::grangertest(dx ~ dy, order = nlag))   # estado -> ref

  tibble(
    estado = estado, coint_johansen = coint_joh, coint_phil_oul = coint_po,
    beta_LP = round(coef(lr)["x"], 3),
    alpha_ajuste = round(alpha, 3), alpha_pval = round(alpha_p, 4),
    alpha_pos = round(a_pos, 3), alpha_neg = round(a_neg, 3),
    assimetria_pval = round(assim_p %||% NA, 4),
    mtar_assim_pval = round(mtar_p %||% NA, 4),
    granger_ref2est = if (!is.null(gx2y)) round(gx2y$`Pr(>F)`[2], 4) else NA,
    granger_est2ref = if (!is.null(gy2x)) round(gy2x$`Pr(>F)`[2], 4) else NA
  )
}
cat("\n=== Tabela 3: Transmissão par a par (estado vs.", rotulo[REFERENCIA], ") ===\n")
tab_par <- map_dfr(ESTADOS, ~ analisar_par(.x, REFERENCIA))
print(as.data.frame(tab_par), row.names = FALSE)
readr::write_csv(tab_par, out_path("tab_transmissao_par.csv"))

# 8. TVECM — VECM com limiar (banda de não-arbitragem) ------------------------
# O QUE: generaliza o ECM permitindo que a velocidade de ajuste MUDE conforme o
# tamanho do desvio do equilíbrio (o termo de correção ECT). Com 1 limiar há 2
# regimes: um "interno" (desvios pequenos — dentro da banda de custos de
# transação, ajuste fraco) e um "externo" (desvios grandes — a arbitragem age,
# ajuste forte). Hansen-Seo (2002) testa H0: VECM linear vs. H1: TVECM (limiar).
# REFERÊNCIA TEÓRICA: Balke & Fomby (1997); Goodwin & Piggott (2001).
tvecm_par <- function(estado, ref = REFERENCIA) {
  Y <- cbind(series_ts[[estado]], series_ts[[ref]]); colnames(Y) <- c(estado, ref)
  m  <- safe(TVECM(Y, lag = 1, nthresh = 1, trim = TVECM_TRIM, plot = FALSE, trace = FALSE))
  hs <- safe(TVECM.HStest(Y, lag = 1, nboot = HS_NBOOT, trim = TVECM_TRIM))
  if (is.null(m)) return(tibble(estado = estado, limiar = NA, beta_LP_tv = NA,
                                ect_interno = NA, ect_externo = NA, n_externo = NA,
                                n_interno = NA, HS_pval = NA))
  eq    <- paste("Equation", estado)
  Bdown <- coef(m)$Bdown; Bup <- coef(m)$Bup
  reg   <- m$model.specific$regime
  # regime 1 (Bdown) = desvios abaixo do limiar; regime 2 (Bup) = acima
  tibble(
    estado     = estado,
    limiar     = round(m$model.specific$Thresh, 4),
    beta_LP_tv = round(-m$model.specific$coint[2, 1], 3),
    ect_regime1 = round(Bdown[eq, "ECT"], 3),     # ajuste no regime inferior
    ect_regime2 = round(Bup[eq, "ECT"], 3),       # ajuste no regime superior
    n_regime1  = sum(reg == 1, na.rm = TRUE),
    n_regime2  = sum(reg == 2, na.rm = TRUE),
    HS_pval    = if (!is.null(hs)) round(hs$PvalBoot, 4) else NA
  )
}
cat("\n=== Tabela 4: TVECM (VECM com limiar) — estado vs.", rotulo[REFERENCIA], "===\n")
tab_tvecm <- map_dfr(ESTADOS, tvecm_par)
print(as.data.frame(tab_tvecm), row.names = FALSE)
readr::write_csv(tab_tvecm, out_path("tab_tvecm.csv"))

# Figura ilustrativa do TVECM (par destacado): ajuste (Δy) vs. desvio defasado
# (ECT_{t-1}), colorido pelo regime, com o limiar estimado. Mostra graficamente
# a banda de não-arbitragem.
fig_tvecm <- safe({
  est <- PAR_ILUSTRA
  y <- as.numeric(series_ts[[est]]); x <- as.numeric(series_ts[[REFERENCIA]])
  Y <- cbind(y, x); colnames(Y) <- c(est, REFERENCIA)
  m <- TVECM(Y, lag = 1, nthresh = 1, trim = TVECM_TRIM, plot = FALSE, trace = FALSE)
  bta <- m$model.specific$coint[2, 1]; th <- m$model.specific$Thresh
  ect <- y + bta * x                       # termo de correção (ECT)
  d <- tibble(ect_lag = head(ect, -1), dY = diff(y)) %>%
    mutate(regime = ifelse(ect_lag <= th, "Regime 1 (desvio baixo)", "Regime 2 (desvio alto)"))
  ggplot(d, aes(ect_lag, dY, color = regime)) +
    geom_vline(xintercept = th, linetype = "dashed") +
    geom_point(alpha = 0.7) + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    labs(title = paste0("TVECM: ajuste de ", rotulo[est], " ao desvio do equilíbrio"),
         subtitle = paste0("Limiar estimado = ", round(th, 4),
                           " | teste de Hansen-Seo rejeita linearidade se p<0,05"),
         x = expression(ECT[t-1]~"(desvio do equilíbrio de longo prazo)"),
         y = expression(Delta~"log preço (estado)"), color = NULL)
})
if (!is.null(fig_tvecm)) salvar_fig(fig_tvecm, "fig04_tvecm_regimes.png")

# 9. Matriz de causalidade de Granger (Figura) --------------------------------
# Heatmap dos p-valores: linha = causa, coluna = efeito. Células escuras
# (p<0,05) indicam que a região-linha "Granger-causa" a região-coluna.
granger_pval <- function(causa, efeito, nlag = NLAG_ECM) {
  g <- safe(lmtest::grangertest(diff(series_ts[[efeito]]) ~ diff(series_ts[[causa]]), order = nlag))
  if (is.null(g)) NA else g$`Pr(>F)`[2]
}
G <- matrix(NA, length(REGIOES), length(REGIOES), dimnames = list(REGIOES, REGIOES))
for (i in REGIOES) for (j in REGIOES) if (i != j) G[i, j] <- granger_pval(i, j)
g_granger <- reshape2::melt(G, varnames = c("causa", "efeito"), value.name = "p") %>%
  filter(!is.na(p)) %>%
  mutate(causa = rotulo[as.character(causa)], efeito = rotulo[as.character(efeito)],
         sig = cut(p, c(-Inf, .01, .05, .1, Inf), labels = c("p<0,01", "p<0,05", "p<0,10", "n.s."))) %>%
  ggplot(aes(efeito, causa, fill = sig)) + geom_tile(color = "white") +
  scale_fill_manual(values = c("p<0,01" = "#08519c", "p<0,05" = "#3182bd",
                               "p<0,10" = "#9ecae1", "n.s." = "grey90"), name = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Causalidade de Granger (causa -> efeito)",
       subtitle = "Células escuras: a região da linha antecede a da coluna",
       x = "efeito", y = "causa")
salvar_fig(g_granger, "fig05_granger_heatmap.png", w = 8.5, h = 6.5)

# 10. VAR / IRF / FEVD --------------------------------------------------------
# VAR em diferenças (séries I(1)). IRF: resposta dos estados a um choque de 1
# desvio-padrão na referência. FEVD: quanto da variância do erro de previsão de
# cada estado é explicada pelo choque na referência (importância da fonte).
dM <- diff(M); colnames(dM) <- make.names(colnames(dM))
REF_S <- make.names(REFERENCIA); EST_S <- make.names(ESTADOS)
var_fit <- safe(VAR(dM, p = max(2, min(4, K)), type = "const"))
fevd_tab <- NULL
if (!is.null(var_fit)) {
  irf_ref <- safe(irf(var_fit, impulse = REF_S, response = EST_S, n.ahead = 12, boot = TRUE, runs = 200))
  if (!is.null(irf_ref)) {
    png(fig_path("fig06_irf.png"), width = 1100, height = 800, res = 110); plot(irf_ref); dev.off()
    message("Figura salva: ", fig_path("fig06_irf.png"))
  }
  fevd_12 <- safe(fevd(var_fit, n.ahead = 12))
  if (!is.null(fevd_12)) {
    fevd_tab <- tibble(estado = ESTADOS,
                       share_ref = sapply(EST_S, function(s) fevd_12[[s]][12, REF_S]),
                       share_propria = sapply(EST_S, function(s) fevd_12[[s]][12, s])) %>%
      mutate(share_outros = pmax(0, 1 - share_ref - share_propria))
    g_fevd <- fevd_tab %>%
      transmute(estado = rotulo[estado],
                `Própria` = share_propria, `Referência` = share_ref, `Outros` = share_outros) %>%
      pivot_longer(-estado, names_to = "fonte", values_to = "share") %>%
      ggplot(aes(reorder(estado, share * (fonte == "Referência")), share, fill = fonte)) +
      geom_col() + coord_flip() + scale_y_continuous(labels = percent) +
      scale_fill_manual(values = c("Própria" = "#fdae6b", "Referência" = "#3182bd", "Outros" = "grey80")) +
      labs(title = "Decomposição da variância (12 meses à frente)",
           subtitle = "Fonte dos choques que explicam o preço de cada estado",
           x = NULL, y = NULL, fill = NULL)
    salvar_fig(g_fevd, "fig07_fevd.png", w = 8.5, h = 5.5)
    readr::write_csv(fevd_tab %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
                     out_path("tab_fevd.csv"))
  }
}

# 11. Relatório consolidado ----------------------------------------------------
readr::write_csv(wide, out_path("painel_precos.csv"))
saveRDS(list(desc = tab_desc, raiz = tab_raiz, par = tab_par, tvecm = tab_tvecm,
             fevd = fevd_tab, johansen = if (!is.null(joh_sys)) summary(joh_sys) else NULL,
             info = list(n = nrow(wide), ini = min(wide$date), fim = max(wide$date),
                         deflac = DEFLACIONAR, log = USAR_LOG, ref = REFERENCIA, K = K)),
        out_path("resultados.rds"))

sink(out_path("relatorio_transmissao.txt"))
cat("================================================================\n")
cat(" TRANSMISSÃO DE PREÇOS DO LEITE — RELATÓRIO\n Gerado em:", format(Sys.time()), "\n")
cat(" Painel:", nrow(wide), "meses (", format(min(wide$date)), "a", format(max(wide$date)), "),",
    length(REGIOES), "regiões.\n Preço:", COL_PRECO, "|",
    ifelse(DEFLACIONAR, "REAL (IPCA)", "NOMINAL"), "| em log |", "Ref.:", REFERENCIA, "\n")
cat("================================================================\n\n")
cat("== TABELA 1: DESCRITIVAS (R$/litro) ==\n"); print(as.data.frame(tab_desc), row.names = FALSE)
cat("\n== TABELA 2: RAIZ UNITÁRIA ==\n");        print(as.data.frame(tab_raiz), row.names = FALSE)
cat("\n== JOHANSEN (sistema) ==\n");              if (!is.null(joh_sys)) print(summary(joh_sys))
cat("\n== TABELA 3: TRANSMISSÃO PAR A PAR ==\n"); print(as.data.frame(tab_par), row.names = FALSE)
cat("\n== TABELA 4: TVECM (LIMIAR) ==\n");        print(as.data.frame(tab_tvecm), row.names = FALSE)
if (!is.null(fevd_tab)) { cat("\n== FEVD (h=12) ==\n"); print(as.data.frame(fevd_tab), row.names = FALSE) }
sink()

cat("\n>>> Análise concluída! Figuras em '", DIR_FIG, "'; tabelas/relatório em '", DIR_OUT, "'.\n", sep = "")
