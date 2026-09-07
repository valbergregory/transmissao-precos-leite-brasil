# =============================================================================
# CONSTRUÇÃO DA BASE DE PREÇOS DO LEITE (CEPEA) — leitura dos .xls e tidy CSV
# =============================================================================
# Os arquivos do CEPEA são .xls (BIFF) que o readxl/libxls NÃO consegue ler.
# Solução: converter para .xlsx com o LibreOffice (headless) e então ler.
#
# Layout de cada arquivo (planilha "Plan 1"):
#   linha 1: título  "...(R$/LITRO) - <REGIÃO>"
#   linha 4: cabeçalho
#   linha 5+: Data(MM/AAAA) | bruto(mín,méd,máx) | líquido(mín,méd,máx)
#   "-" e "0,0000" são marcadores de AUSÊNCIA. Decimal com vírgula.
#
# Saída: dados/cepea_leite_regioes.csv (long, uma linha por região×mês).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr); library(purrr)
})

# 1. Parâmetros ---------------------------------------------------------------
INPUT_DIR  <- file.path("dados", "brutos_cepea")  # pasta com os cepea-consulta-*.xls
PADRAO     <- "cepea-consulta-.*\\.xls$"
SAIDA_CSV  <- file.path("dados", "cepea_leite_regioes.csv")

# Localiza o LibreOffice (soffice) -------------------------------------------
achar_soffice <- function() {
  cands <- c("C:/Program Files/LibreOffice/program/soffice.exe",
             "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
             Sys.which("soffice"))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands) == 0) stop("LibreOffice (soffice) não encontrado. Instale-o ou ajuste o caminho.")
  cands[1]
}

# 2. Conversão .xls -> .xlsx (LibreOffice headless) ---------------------------
xls <- list.files(INPUT_DIR, pattern = PADRAO, full.names = TRUE, ignore.case = TRUE)
if (length(xls) == 0) stop("Nenhum .xls do CEPEA encontrado em ", INPUT_DIR)

tmp_xlsx <- file.path(tempdir(), "cepea_xlsx"); dir.create(tmp_xlsx, showWarnings = FALSE)
soffice <- achar_soffice()
message("Convertendo ", length(xls), " arquivos com LibreOffice...")
args <- c("--headless", "--convert-to", "xlsx", "--outdir", tmp_xlsx, xls)
system2(soffice, args = shQuote(args), stdout = FALSE, stderr = FALSE)
Sys.sleep(1)
xlsx <- list.files(tmp_xlsx, pattern = "\\.xlsx$", full.names = TRUE)
if (length(xlsx) == 0) stop("Conversão falhou — nenhum .xlsx gerado em ", tmp_xlsx)

# 3. Leitura de um arquivo ----------------------------------------------------
# "-", vazio e 0 são marcadores de ausência
num <- function(x) {
  x <- trimws(as.character(x)); x[x %in% c("-", "", "NA")] <- NA
  v <- as.numeric(gsub(",", ".", x)); v[!is.na(v) & v == 0] <- NA; v
}

# Detecta a região varrendo as primeiras linhas em busca de "(R$/LITRO) - <REGIÃO>"
detectar_regiao <- function(raw_head) {
  txt <- unlist(raw_head, use.names = FALSE)
  hit <- str_match(txt, "R\\$\\s*/\\s*LITRO\\)\\s*-\\s*(.+?)\\s*$")[, 2]
  reg <- hit[!is.na(hit)][1]
  if (is.na(reg)) return(NA_character_)
  reg <- str_replace(reg, "\\s*-\\s*Consulta.*$", "")
  # normaliza acentos -> ASCII para usar como chave estável
  iconv(trimws(reg), to = "ASCII//TRANSLIT")
}

ler_um <- function(arq) {
  raw <- read_excel(arq, sheet = "Plan 1", col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  regiao <- detectar_regiao(raw[1:min(4, nrow(raw)), ])
  if (is.na(regiao)) { warning("Região não detectada em ", basename(arq)); return(NULL) }
  while (ncol(raw) < 7) raw[[paste0("x", ncol(raw) + 1)]] <- NA
  raw <- raw[, 1:7]
  names(raw) <- c("data_txt", "bruto_min", "bruto_med", "bruto_max",
                  "liq_min", "liq_med", "liq_max")
  raw %>%
    filter(grepl("^\\d{2}/\\d{4}$", data_txt)) %>%
    transmute(
      regiao = regiao,
      date   = as.Date(paste0(substr(data_txt, 4, 7), "-", substr(data_txt, 1, 2), "-01")),
      bruto_min = num(bruto_min), bruto_med = num(bruto_med), bruto_max = num(bruto_max),
      liq_min   = num(liq_min),   liq_med   = num(liq_med),   liq_max   = num(liq_max)
    )
}

# 4. Consolidação -------------------------------------------------------------
todos <- map_dfr(xlsx, ler_um) %>%
  distinct(regiao, date, .keep_all = TRUE) %>%      # de-duplica downloads repetidos
  arrange(regiao, date)

dir.create(dirname(SAIDA_CSV), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(todos, SAIDA_CSV)

message("CSV salvo: ", normalizePath(SAIDA_CSV))
cat("\nCobertura por região (preço líquido médio):\n")
todos %>% group_by(regiao) %>%
  summarise(n = n(), ini = min(date), fim = max(date),
            na_liq_med = sum(is.na(liq_med)), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)
