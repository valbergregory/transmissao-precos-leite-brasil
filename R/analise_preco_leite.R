# =============================================================================
# ANÁLISE AVANÇADA DO PREÇO REAL DO LEITE – API IPEA + INTERATIVIDADE + ML
# =============================================================================
# Coleta de dados via API REST OData do IPEA (dispensa o pacote ipeadatar).
# Inclui: gráficos plotly, testes robustos de raiz unitária e não linearidade,
# quebras estruturais (Bai-Perron), ARIMAX, BSTS, XGBoost, validação cruzada
# temporal (expanding window), intervalos por bootstrap e relatório.
#
# NOTAS IMPORTANTES SOBRE A API DO IPEA (validadas em 2026):
#   * A base correta é  http://www.ipeadata.gov.br/api/odata4/
#   * O servidor OData do IPEA NÃO suporta $filter/$select/contains/substringof
#     de forma confiável (devolve erro). Por isso baixamos o catálogo completo
#     de metadados UMA vez (cacheado) e filtramos localmente com grepl().
#   * Códigos confirmados:
#       - Leite (preço ao produtor): "DERAL12_PRLECO12"
#         (Preço médio recebido pelo agricultor – leite – litro – PR; mensal)
#         OBS.: é a única série de PREÇO de leite disponível no IPEA. A série
#         nacional do CEPEA/ESALQ NÃO está no IPEA — para usá-la, baixe o CSV
#         do CEPEA e aponte CAMINHO_LEITE_CSV abaixo.
#       - Deflator IPCA (número-índice, dez/1993 = 100): "PRECOS12_IPCA12"
#       - IPCA variação mensal (%):                      "PRECOS12_IPCAG12"
# =============================================================================

# 0. Pacotes ------------------------------------------------------------------
required_pkgs <- c(
  "tidyverse", "lubridate", "zoo", "forecast", "urca", "tseries", "uroot",
  "strucchange", "lmtest", "sandwich", "broom", "scales", "ggplot2", "plotly",
  "htmlwidgets", "bsts", "xgboost", "caret", "timetk",
  "httr", "jsonlite", "purrr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando pacote: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
}
invisible(lapply(required_pkgs, install_if_missing))
suppressPackageStartupMessages(
  invisible(lapply(required_pkgs, library, character.only = TRUE))
)

theme_set(theme_minimal(base_size = 14))
set.seed(123)

# Pasta de saída --------------------------------------------------------------
DIR_OUT <- "resultados"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
out_path <- function(f) file.path(DIR_OUT, f)

# Salva widget plotly de forma robusta (pandoc/htmlwidgets podem faltar) ------
salvar_widget <- function(widget, arquivo) {
  ok <- tryCatch({
    htmlwidgets::saveWidget(widget, out_path(arquivo), selfcontained = TRUE)
    TRUE
  }, error = function(e) {
    # selfcontained exige pandoc; tenta sem auto-empacotar
    tryCatch({
      htmlwidgets::saveWidget(widget, out_path(arquivo), selfcontained = FALSE)
      TRUE
    }, error = function(e2) {
      message("[aviso] Não foi possível salvar ", arquivo, ": ", conditionMessage(e2))
      FALSE
    })
  })
  if (ok) message("Gráfico salvo: ", out_path(arquivo))
  invisible(ok)
}

# 1. Parâmetros ---------------------------------------------------------------
indice_inflacao <- "IPCA"
data_inicio     <- as.Date("2010-01-01")
usar_ultimos_n  <- 180                 # NULL = usar tudo
K_fourier       <- 4
h_previsao      <- 12
n_bootstrap     <- 1000                # nº de trajetórias bootstrap p/ intervalos

# Códigos das séries no IPEA (NULL => busca automática no catálogo) -----------
CODIGO_LEITE <- "DERAL12_PRLECO12"     # preço do leite ao produtor (PR), mensal
CODIGO_IPCA  <- "PRECOS12_IPCA12"      # IPCA número-índice (dez/1993 = 100)

# Alternativa: usar um CSV do CEPEA para o preço do leite (NULL = usar IPEA).
# O CSV deve ter uma coluna de data e uma de preço (ajuste em ler_leite_csv).
CAMINHO_LEITE_CSV <- NULL

eventos_normativos <- tribble(
  ~evento,              ~data_evento,  ~tipo,          ~usar_modelo,
  "IN62_2011",          "2012-01-01",  "sanitaria",    TRUE,
  "IN07_2016",          "2016-05-01",  "sanitaria",    TRUE,
  "RIISPOA_2017",       "2017-04-01",  "inspecao",     TRUE,
  "IN76_IN77_2018",     "2018-12-01",  "sanitaria",    TRUE,
  "IN58_IN59_2019",     "2019-11-01",  "sanitaria",    TRUE,
  "PGPM_2023",          "2023-07-01",  "preco_minimo", TRUE,
  "Dec11732_2023_2024", "2024-02-01",  "tributaria",   TRUE
) %>% mutate(data_evento = as.Date(data_evento))

# 2. Camada de acesso à API IPEA ----------------------------------------------
base_url  <- "http://www.ipeadata.gov.br/api/odata4/"
cache_dir <- "cache_ipea"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

# GET com timeout, user-agent e retry (robusto a instabilidade da API) --------
get_ipea <- function(endpoint) {
  url <- paste0(base_url, endpoint)
  resp <- httr::RETRY(
    "GET", url,
    httr::user_agent("analise-preco-leite-R (pesquisa academica)"),
    httr::timeout(120),
    times = 4, pause_base = 2, pause_cap = 30, quiet = TRUE
  )
  if (httr::http_error(resp)) {
    stop("Erro HTTP ", httr::status_code(resp), " ao acessar: ", url)
  }
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
  if (!is.null(parsed$error)) {
    stop("API IPEA retornou erro: ",
         parsed$error$message %||% "desconhecido", " (", url, ")")
  }
  parsed$value
}

# Baixa (e cacheia) o catálogo completo de metadados --------------------------
catalogo_ipea <- function(forcar = FALSE) {
  cache_file <- file.path(cache_dir, "catalogo_metadados.rds")
  if (file.exists(cache_file) && !forcar) {
    cat_meta <- readRDS(cache_file)
    # invalida cache com mais de 30 dias
    if (difftime(Sys.time(), file.info(cache_file)$mtime, units = "days") < 30) {
      return(cat_meta)
    }
  }
  message("Baixando catálogo de metadados do IPEA (pode levar alguns segundos)...")
  cat_meta <- get_ipea("Metadados")
  cat_meta <- as_tibble(cat_meta)
  saveRDS(cat_meta, cache_file)
  cat_meta
}

# Busca o código de uma série filtrando o catálogo localmente -----------------
# termo/filtro são regex aplicadas a SERNOME; preferimos séries ATIVAS e mensais.
buscar_codigo_serie <- function(termo, filtro = NULL, periodicidade = "Mensal") {
  cat_meta <- catalogo_ipea()
  meta <- cat_meta %>%
    filter(grepl(termo, SERNOME, ignore.case = TRUE))
  if (!is.null(filtro)) {
    idx <- grepl(filtro, meta$SERNOME, ignore.case = TRUE)
    if (any(idx)) meta <- meta[idx, ]
  }
  if (!is.null(periodicidade) && "PERNOME" %in% names(meta)) {
    idx <- grepl(periodicidade, meta$PERNOME, ignore.case = TRUE)
    if (any(idx)) meta <- meta[idx, ]
  }
  # evita séries marcadas como inativas, quando possível
  if (any(!grepl("INATIVA", meta$SERNOME, ignore.case = TRUE))) {
    meta <- meta[!grepl("INATIVA", meta$SERNOME, ignore.case = TRUE), ]
  }
  if (nrow(meta) == 0) stop("Nenhuma série encontrada para o termo: ", termo)
  meta <- arrange(meta, SERCODIGO)
  message("Série selecionada: ", meta$SERNOME[1], " (", meta$SERCODIGO[1], ")")
  if (nrow(meta) > 1) {
    message("  (", nrow(meta), " candidatas; demais: ",
            paste(head(meta$SERCODIGO[-1], 5), collapse = ", "), " ...)")
  }
  meta$SERCODIGO[1]
}

# Baixa (e cacheia) os valores de uma série -----------------------------------
baixar_serie_ipea <- function(codigo) {
  cache_file <- file.path(cache_dir, paste0("serie_", make.names(codigo), ".rds"))
  if (file.exists(cache_file) &&
      difftime(Sys.time(), file.info(cache_file)$mtime, units = "days") < 7) {
    return(readRDS(cache_file))
  }
  endpoint <- paste0("ValoresSerie(SERCODIGO='", utils::URLencode(codigo), "')")
  valores  <- get_ipea(endpoint)
  if (is.null(valores) || length(valores) == 0 || nrow(valores) == 0) {
    stop("Série ", codigo, " não retornou valores.")
  }
  valores <- valores %>%
    transmute(
      date  = as.Date(substr(VALDATA, 1, 10)),
      value = suppressWarnings(as.numeric(VALVALOR))
    ) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date)
  saveRDS(valores, cache_file)
  valores
}

obter_serie <- function(termo, filtro = NULL, codigo_manual = NULL) {
  codigo <- if (!is.null(codigo_manual)) {
    message("Usando código informado: ", codigo_manual)
    codigo_manual
  } else {
    buscar_codigo_serie(termo, filtro)
  }
  baixar_serie_ipea(codigo)
}

# Leitor opcional de CSV do CEPEA --------------------------------------------
ler_leite_csv <- function(caminho) {
  message("Lendo preço do leite a partir de CSV: ", caminho)
  raw <- readr::read_csv2(caminho, show_col_types = FALSE) %>%
    janitor_clean_names()
  # heurística: 1ª coluna de data, 1ª coluna numérica = preço
  col_data  <- names(raw)[which(sapply(raw, function(x)
    inherits(x, "Date") || all(grepl("\\d{2}/\\d{4}|\\d{4}-\\d{2}", as.character(x)))))[1]]
  col_preco <- names(raw)[which(sapply(raw, is.numeric))[1]]
  raw %>%
    transmute(
      date  = lubridate::parse_date_time(.data[[col_data]],
                                         orders = c("dmy", "my", "Ymd", "ym")) %>% as.Date(),
      value = .data[[col_preco]]
    ) %>%
    filter(!is.na(date), !is.na(value)) %>%
    mutate(date = lubridate::floor_date(date, "month")) %>%
    arrange(date)
}
# pequeno helper p/ não depender do janitor
janitor_clean_names <- function(df) {
  names(df) <- names(df) %>% tolower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    gsub("[^a-z0-9]+", "_", .) %>% gsub("^_|_$", "", .)
  df
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Converte série de variação (%) em número-índice, se necessário --------------
criar_indice_deflator <- function(df_indice) {
  med <- stats::median(abs(df_indice$value), na.rm = TRUE)
  mx  <- max(abs(df_indice$value), na.rm = TRUE)
  if (med < 5 && mx < 30) {
    message("Deflator: detectada variação mensal (%); convertendo para índice.")
    df_indice %>% arrange(date) %>%
      mutate(indice = 100 * cumprod(1 + value / 100)) %>%
      select(date, indice)
  } else {
    message("Deflator: usando série como número-índice de nível.")
    df_indice %>% transmute(date, indice = value)
  }
}

# Dummies de eventos: D_ = degrau (level shift); P_ = pulso pontual -----------
criar_dummies_eventos <- function(df, eventos, usar = TRUE) {
  ev  <- eventos %>% filter(usar_modelo == usar)
  out <- df
  for (i in seq_len(nrow(ev))) {
    nome <- make.names(ev$evento[i])
    d    <- ev$data_evento[i]
    out[[paste0("D_", nome)]] <- as.numeric(out$date >= d)
    out[[paste0("P_", nome)]] <- as.numeric(
      lubridate::floor_date(out$date, "month") == lubridate::floor_date(d, "month"))
  }
  out
}

# Remove colunas constantes de uma matriz (evita singularidade no ARIMAX) -----
drop_const_cols <- function(M) {
  if (is.null(M) || ncol(M) == 0) return(M)
  keep <- apply(M, 2, function(z) length(unique(z[!is.na(z)])) > 1)
  M[, keep, drop = FALSE]
}

# 3. Coleta dos dados ---------------------------------------------------------
message("\n== Coletando dados ==")
df_leite <- if (!is.null(CAMINHO_LEITE_CSV)) {
  ler_leite_csv(CAMINHO_LEITE_CSV)
} else {
  obter_serie("leite", filtro = "produtor|agricultor|recebido",
              codigo_manual = CODIGO_LEITE)
} %>% rename(preco_nominal = value)

df_infla_raw <- obter_serie(indice_inflacao, filtro = "geral",
                            codigo_manual = CODIGO_IPCA)
df_infla <- criar_indice_deflator(df_infla_raw)

# 4. Preparação ----------------------------------------------------------------
dados <- df_leite %>%
  mutate(date = lubridate::floor_date(date, "month")) %>%
  inner_join(df_infla %>% mutate(date = lubridate::floor_date(date, "month")),
             by = "date") %>%
  arrange(date) %>%
  filter(date >= data_inicio)

if (nrow(dados) < 60)
  stop("Amostra muito curta (", nrow(dados),
       " obs). Verifique os códigos das séries / o período inicial.")

# Grade mensal contínua + interpolação de eventuais buracos -------------------
dados <- dados %>%
  complete(date = seq(min(date), max(date), by = "month")) %>%
  arrange(date) %>%
  mutate(
    preco_nominal = zoo::na.approx(preco_nominal, na.rm = FALSE),
    indice        = zoo::na.approx(indice,        na.rm = FALSE)
  ) %>%
  drop_na(preco_nominal, indice)

if (!is.null(usar_ultimos_n) && nrow(dados) > usar_ultimos_n) {
  dados <- tail(dados, usar_ultimos_n)
}

# Preço real: base = último período da amostra (preços de hoje) ---------------
indice_base <- last(dados$indice)
dados <- dados %>%
  mutate(
    preco_real         = preco_nominal * indice_base / indice,
    log_preco_real     = log(preco_real),
    inflacao_acumulada = indice / first(indice) - 1,
    mes                = factor(month(date), levels = 1:12),
    tendencia          = row_number()
  ) %>%
  criar_dummies_eventos(eventos_normativos, usar = TRUE)

message(sprintf("Amostra final: %d meses (%s a %s).",
                nrow(dados), format(min(dados$date)), format(max(dados$date))))

# Série em log, em formato ts -------------------------------------------------
y <- ts(dados$log_preco_real,
        start = c(year(min(dados$date)), month(min(dados$date))), frequency = 12)
y_clean <- forecast::tsclean(y)   # versão sem outliers/imputada (uso opcional)

# 5. Gráficos descritivos interativos -----------------------------------------
g_preco <- ggplot(dados, aes(date, preco_real)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_vline(data = eventos_normativos %>% filter(usar_modelo),
             aes(xintercept = data_evento), linetype = "dashed", alpha = 0.5) +
  labs(title = "Preço real do leite (deflacionado pelo IPCA)",
       x = NULL, y = "R$/litro (preços do último mês)")
salvar_widget(ggplotly(g_preco, dynamicTicks = TRUE), "01_preco_real_interativo.html")

decomp <- stl(y, s.window = "periodic", robust = TRUE)
df_stl <- timetk::tk_tbl(decomp$time.series, rename_index = "idx") %>%
  mutate(date = dados$date) %>%
  select(-any_of("idx")) %>%
  pivot_longer(-date, names_to = "componente", values_to = "valor")
g_stl <- ggplot(df_stl, aes(date, valor, color = componente)) +
  geom_line() +
  facet_wrap(~componente, scales = "free_y", ncol = 1) +
  theme(legend.position = "none") +
  labs(title = "Decomposição STL do log do preço real", x = NULL, y = NULL)
salvar_widget(ggplotly(g_stl), "02_stl_interativo.html")

# 6. Testes de raiz unitária e não linearidade --------------------------------
safe_test <- function(expr) tryCatch(expr, error = function(e) {
  message("[aviso] teste falhou: ", conditionMessage(e)); NULL
})

adf_nivel <- safe_test(ur.df(y, type = "trend", selectlags = "AIC"))
adf_diff  <- safe_test(ur.df(diff(y), type = "drift", selectlags = "AIC"))
kpss      <- safe_test(ur.kpss(y, type = "tau"))
pp        <- safe_test(ur.pp(y, type = "Z-tau", model = "trend", lags = "short"))
za        <- safe_test(ur.za(y, model = "both",
                             lag = max(2, floor(length(y)^(1/3)))))
# HEGY (sazonalidade) vem do pacote 'uroot', não do 'urca'
hegy  <- safe_test(uroot::hegy.test(y, deterministic = c(1, 1, 1)))
teras <- safe_test(tseries::terasvirta.test(y, type = "Chisq"))

cat("\n=== ADF (nível) ===\n");      if (!is.null(adf_nivel)) print(summary(adf_nivel))
cat("\n=== KPSS ===\n");             if (!is.null(kpss))      print(summary(kpss))
cat("\n=== Zivot-Andrews ===\n");    if (!is.null(za))        print(summary(za))
cat("\n=== HEGY (sazonal) ===\n");   if (!is.null(hegy))      print(hegy)
cat("\n=== Terasvirta (não linearidade) ===\n"); if (!is.null(teras)) print(teras)

# 7. Quebras estruturais (Bai-Perron) -----------------------------------------
bp <- safe_test(breakpoints(log_preco_real ~ tendencia + mes, data = dados, h = 0.15))
n_breaks_opt  <- 0
datas_quebras <- as.Date(character(0))
if (!is.null(bp)) {
  bic_vec      <- BIC(bp)
  n_breaks_opt <- as.integer(names(which.min(bic_vec)))
  if (is.na(n_breaks_opt)) n_breaks_opt <- which.min(bic_vec) - 1L
  cat("\nNúmero ótimo de quebras (BIC):", n_breaks_opt, "\n")

  if (n_breaks_opt > 0) {
    idx_quebras   <- breakpoints(bp, breaks = n_breaks_opt)$breakpoints
    datas_quebras <- dados$date[idx_quebras]
    bp_fit <- fitted(bp, breaks = n_breaks_opt)
    df_bp  <- tibble(date = dados$date,
                     original = as.numeric(y),
                     ajustado = as.numeric(bp_fit))
    g_bp <- ggplot(df_bp, aes(date)) +
      geom_line(aes(y = original), color = "gray50") +
      geom_line(aes(y = ajustado), color = "red", linewidth = 1) +
      geom_vline(xintercept = datas_quebras, linetype = "dashed") +
      labs(title = paste0("Quebras estruturais (Bai-Perron, n = ", n_breaks_opt, ")"),
           x = NULL, y = "Log preço real")
    salvar_widget(ggplotly(g_bp), "03_quebras_interativo.html")
    cat("Datas das quebras:", paste(format(datas_quebras), collapse = ", "), "\n")
  }
}

# 8. Matrizes de regressores ---------------------------------------------------
cols_dummies  <- names(dados)[grepl("^D_", names(dados))]
xreg_eventos  <- as.matrix(dados[, cols_dummies, drop = FALSE])
fourier_terms <- forecast::fourier(y, K = K_fourier)   # length(y) x 2K

# Mantém apenas regressores não constantes na amostra cheia (p/ modelos finais)
xreg_full   <- cbind(xreg_eventos, fourier_terms)
keep_full   <- apply(xreg_full, 2, function(z) length(unique(z)) > 1)
xreg_full   <- xreg_full[, keep_full, drop = FALSE]

# 9. Modelos finais (amostra completa) ----------------------------------------
message("\n== Ajustando modelos finais ==")
arima_base <- auto.arima(y, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
arimax     <- auto.arima(y, xreg = xreg_full, seasonal = FALSE,
                         stepwise = FALSE, approximation = FALSE)

# bst (modelo estrutural bayesiano) com regressores
model_bst <- safe_test({
  ss <- AddLocalLinearTrend(list(), y)
  ss <- AddSeasonal(ss, y, nseasons = 12)
  bst(y ~ xreg_full, state.specification = ss, niter = 1000, ping = 0, seed = 123)
})

# 10. XGBoost: previsor recursivo (multi-step) --------------------------------
# Matriz de regressores EXÓGENOS (sem lags), alinhada por índice temporal,
# usada tanto no treino quanto na previsão recursiva.
build_exo <- function(n_obs, fourier_mat, eventos_mat, trend, mes_num) {
  data.frame(
    trend   = trend,
    mes_sin = sin(2 * pi * mes_num / 12),
    mes_cos = cos(2 * pi * mes_num / 12),
    eventos_mat,
    fourier_mat,
    check.names = FALSE
  )
}

xgb_params <- list(objective = "reg:squarederror", eta = 0.05,
                   max_depth = 4, subsample = 0.9, colsample_bytree = 0.9)

# Treina em train_idx e prevê recursivamente em fut_idx.
# y_vec: vetor completo (NA permitido nas posições futuras);
# exo_all: data.frame de exógenos com nrow == length(y_vec).
xgb_forecast <- function(y_vec, exo_all, train_idx, fut_idx,
                         params = xgb_params, nrounds = 400) {
  feat <- exo_all
  feat$lag1  <- dplyr::lag(y_vec, 1)
  feat$lag2  <- dplyr::lag(y_vec, 2)
  feat$lag12 <- dplyr::lag(y_vec, 12)
  feat_cols  <- colnames(feat)

  tr <- intersect(train_idx, which(stats::complete.cases(feat)))
  if (length(tr) < 30) stop("Poucos dados para treinar XGBoost.")
  dtrain <- xgboost::xgb.DMatrix(as.matrix(feat[tr, , drop = FALSE]),
                                 label = y_vec[tr])
  bst <- xgboost::xgb.train(params, dtrain, nrounds = nrounds, verbose = 0)

  yext  <- y_vec
  preds <- numeric(length(fut_idx))
  for (i in seq_along(fut_idx)) {
    t  <- fut_idx[i]
    xv <- c(as.numeric(exo_all[t, ]),
            lag1  = yext[t - 1],
            lag2  = yext[t - 2],
            lag12 = yext[t - 12])
    names(xv)[seq_len(ncol(exo_all))] <- colnames(exo_all)
    xm <- matrix(xv[feat_cols], nrow = 1, dimnames = list(NULL, feat_cols))
    p  <- as.numeric(predict(bst, xgboost::xgb.DMatrix(xm)))
    preds[i] <- p
    yext[t]  <- p
  }
  list(preds = preds, model = bst)
}

exo_full <- build_exo(nrow(dados), fourier_terms, xreg_eventos,
                      dados$tendencia, as.numeric(as.character(dados$mes)))

# 11. Validação cruzada temporal (expanding window) ---------------------------
message("\n== Validação cruzada temporal (expanding window) ==")
horizonte_teste <- 12
y_vec   <- as.numeric(y)
n_total <- length(y_vec)
train_sizes <- seq(60, n_total - horizonte_teste, by = 6)

acc <- list(arimax = c(), bst = c(), xgb = c(), real = c())
na_h <- rep(NA_real_, horizonte_teste)

for (end_train in train_sizes) {
  idx_train <- seq_len(end_train)
  idx_test  <- (end_train + 1):(end_train + horizonte_teste)

  # Regressores do treino/teste, mantendo apenas colunas não constantes no treino
  xtr <- cbind(xreg_eventos[idx_train, , drop = FALSE], fourier_terms[idx_train, ])
  keep <- apply(xtr, 2, function(z) length(unique(z)) > 1)
  xtr  <- xtr[, keep, drop = FALSE]
  xte  <- cbind(xreg_eventos[idx_test, , drop = FALSE],
                fourier_terms[idx_test, ])[, keep, drop = FALSE]

  ytr <- ts(y_vec[idx_train], frequency = 12)

  # --- ARIMAX ---
  p_ar <- tryCatch({
    fit <- auto.arima(ytr, xreg = xtr, seasonal = FALSE,
                      stepwise = TRUE, approximation = TRUE)
    as.numeric(forecast(fit, xreg = xte, h = horizonte_teste)$mean)
  }, error = function(e) na_h)

  # --- bst ---
  p_b <- tryCatch({
    ss <- AddLocalLinearTrend(list(), ytr)
    ss <- AddSeasonal(ss, ytr, nseasons = 12)
    fit <- bst(y_vec[idx_train] ~ xtr, state.specification = ss,
                niter = 300, ping = 0, seed = 123)
    pb <- predict(fit, newdata = xte, horizon = horizonte_teste, quantiles = c(.025, .975))
    as.numeric(pb$mean)
  }, error = function(e) na_h)

  # --- XGBoost (recursivo) ---
  p_x <- tryCatch({
    yv <- y_vec; yv[idx_test] <- NA
    xgb_forecast(yv, exo_full, idx_train, idx_test, nrounds = 300)$preds
  }, error = function(e) na_h)

  acc$arimax <- c(acc$arimax, p_ar)
  acc$bst   <- c(acc$bst,   p_b)
  acc$xgb    <- c(acc$xgb,    p_x)
  acc$real   <- c(acc$real,   y_vec[idx_test])
}

# Métricas (somente pares válidos) --------------------------------------------
metricas <- function(pred, real) {
  ok <- is.finite(pred) & is.finite(real)
  if (!any(ok)) return(c(MAE = NA, RMSE = NA, MAPE = NA))
  e <- pred[ok] - real[ok]
  c(MAE  = mean(abs(e)),
    RMSE = sqrt(mean(e^2)),
    MAPE = mean(abs(e / real[ok])) * 100)
}

pred_ensemble <- rowMeans(cbind(acc$arimax, acc$bst, acc$xgb), na.rm = TRUE)
tab_cv <- rbind(
  ARIMAX   = metricas(acc$arimax,   acc$real),
  bst     = metricas(acc$bst,     acc$real),
  XGBoost  = metricas(acc$xgb,      acc$real),
  Ensemble = metricas(pred_ensemble, acc$real)
)
cat("\n=== Desempenho na validação cruzada ===\n")
print(round(tab_cv, 4))

# Teste de Diebold-Mariano (ARIMAX vs XGBoost), se houver dados ---------------
dm <- safe_test({
  ok <- is.finite(acc$arimax) & is.finite(acc$xgb) & is.finite(acc$real)
  forecast::dm.test(acc$arimax[ok] - acc$real[ok],
                    acc$xgb[ok] - acc$real[ok], h = horizonte_teste, power = 2)
})
if (!is.null(dm)) { cat("\n=== Diebold-Mariano (ARIMAX vs XGBoost) ===\n"); print(dm) }

# 12. Previsão final + intervalos por bootstrap -------------------------------
message("\n== Previsão final (", h_previsao, " meses) ==")
datas_futuras <- seq(max(dados$date) %m+% months(1), by = "month",
                     length.out = h_previsao)
futuro <- tibble(date = datas_futuras) %>%
  mutate(mes       = factor(month(date), levels = 1:12),
         tendencia = max(dados$tendencia) + row_number()) %>%
  criar_dummies_eventos(eventos_normativos, usar = TRUE)

xreg_fut_eventos <- as.matrix(futuro[, cols_dummies, drop = FALSE])
fourier_futuro   <- forecast::fourier(y, K = K_fourier, h = h_previsao)
# Reproduz, no futuro, exatamente as colunas mantidas no modelo final
xreg_futuro <- cbind(xreg_fut_eventos, fourier_futuro)[, keep_full, drop = FALSE]
stopifnot(identical(colnames(xreg_futuro), colnames(xreg_full)))

# forecast() já produz intervalos por bootstrap (reamostragem dos resíduos)
prev <- forecast(arimax, xreg = xreg_futuro, h = h_previsao,
                 level = 95, bootstrap = TRUE, npaths = n_bootstrap)

# Previsão alternativa por XGBoost (recursiva) para comparação ----------------
exo_fut <- build_exo(h_previsao, fourier_futuro, xreg_fut_eventos,
                     futuro$tendencia, as.numeric(as.character(futuro$mes)))
exo_ext <- rbind(exo_full, exo_fut)
y_ext   <- c(y_vec, rep(NA_real_, h_previsao))
prev_xgb <- safe_test(
  xgb_forecast(y_ext, exo_ext, seq_len(n_total),
               (n_total + 1):(n_total + h_previsao), nrounds = 400)$preds
)

# Gráfico final interativo ----------------------------------------------------
df_prev <- tibble(
  date     = c(dados$date, datas_futuras),
  observado = c(as.numeric(y), rep(NA, h_previsao)),
  arimax    = c(rep(NA, nrow(dados)), as.numeric(prev$mean)),
  lower     = c(rep(NA, nrow(dados)), as.numeric(prev$lower)),
  upper     = c(rep(NA, nrow(dados)), as.numeric(prev$upper)),
  xgboost   = c(rep(NA, nrow(dados)),
                if (!is.null(prev_xgb)) prev_xgb else rep(NA, h_previsao))
)
g_prev <- ggplot(df_prev, aes(date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue",
              alpha = 0.5, na.rm = TRUE) +
  geom_line(aes(y = observado), color = "gray40") +
  geom_line(aes(y = arimax),  color = "red",  linewidth = 1, na.rm = TRUE) +
  geom_line(aes(y = xgboost), color = "darkgreen", linewidth = 0.8,
            linetype = "dashed", na.rm = TRUE) +
  labs(title = "Previsão: ARIMAX (vermelho) e XGBoost (verde) — banda bootstrap 95%",
       x = NULL, y = "Log preço real")
salvar_widget(ggplotly(g_prev), "04_previsao_interativa.html")

# 13. Exportação de resultados -------------------------------------------------
sink(out_path("resultados_modelos.txt"))
cat("=============================================================\n")
cat(" ANÁLISE DO PREÇO REAL DO LEITE — RELATÓRIO DE RESULTADOS\n")
cat(" Gerado em:", format(Sys.time()), "\n")
cat(" Amostra:", nrow(dados), "meses (",
    format(min(dados$date)), "a", format(max(dados$date)), ")\n")
cat(" Série leite:", ifelse(is.null(CAMINHO_LEITE_CSV), CODIGO_LEITE, CAMINHO_LEITE_CSV),
    "| Deflator:", CODIGO_IPCA, "\n")
cat("=============================================================\n\n")

cat("===== TESTES DE RAIZ UNITÁRIA / NÃO LINEARIDADE =====\n")
if (!is.null(adf_nivel)) { cat("\n-- ADF nível --\n"); print(summary(adf_nivel)) }
if (!is.null(adf_diff))  { cat("\n-- ADF 1ª dif --\n"); print(summary(adf_diff)) }
if (!is.null(kpss))      { cat("\n-- KPSS --\n");        print(summary(kpss)) }
if (!is.null(pp))        { cat("\n-- Phillips-Perron --\n"); print(summary(pp)) }
if (!is.null(za))        { cat("\n-- Zivot-Andrews --\n");   print(summary(za)) }
if (!is.null(hegy))      { cat("\n-- HEGY (sazonal) --\n");  print(hegy) }
if (!is.null(teras))     { cat("\n-- Terasvirta --\n");      print(teras) }

cat("\n\n===== QUEBRAS ESTRUTURAIS (Bai-Perron) =====\n")
if (!is.null(bp)) print(summary(bp))
cat("Número ótimo de quebras:", n_breaks_opt, "\n")
if (n_breaks_opt > 0)
  cat("Datas:", paste(format(datas_quebras), collapse = ", "), "\n")

cat("\n\n===== MODELOS FINAIS =====\n")
cat("ARIMA base :", forecast::arimaorder(arima_base), "| AICc =",
    round(arima_base$aicc, 2), "\n")
cat("ARIMAX     :", forecast::arimaorder(arimax), "| AICc =",
    round(arimax$aicc, 2), "\n")

cat("\n\n===== VALIDAÇÃO CRUZADA (expanding window, h =", horizonte_teste, ") =====\n")
print(round(tab_cv, 4))
if (!is.null(dm)) { cat("\nDiebold-Mariano (ARIMAX vs XGBoost):\n"); print(dm) }

cat("\n\n===== PREVISÃO ARIMAX (", h_previsao, "meses, log preço real) =====\n")
print(data.frame(
  data  = format(datas_futuras),
  media = round(as.numeric(prev$mean), 4),
  lower = round(as.numeric(prev$lower), 4),
  upper = round(as.numeric(prev$upper), 4)
))
sink()

# Salva também a base tratada e as previsões em CSV ---------------------------
readr::write_csv(dados, out_path("dados_tratados.csv"))
readr::write_csv(df_prev, out_path("previsoes.csv"))

cat("\n>>> Análise concluída! Resultados salvos em '", normalizePath(DIR_OUT), "'.\n", sep = "")
