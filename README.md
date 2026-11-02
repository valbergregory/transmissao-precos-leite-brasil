# Transmissão de Preços do Leite ao Produtor entre Regiões Brasileiras

Análise da integração e da transmissão espacial de preços do leite ao produtor
entre regiões do Brasil, com séries mensais do CEPEA/ESALQ-USP (jan/2016 a
abr/2026) deflacionadas pelo IPCA. O trabalho aplica o arcabouço de
transmissão de preços agrícolas: raiz unitária, cointegração, modelos de
correção de erro (simétrico e assimétrico), cointegração com limiar e VECM
com limiar (TVECM), causalidade de Granger e VAR/IRF/FEVD.

Perguntas de pesquisa: os preços regionais compartilham uma relação de
equilíbrio de longo prazo; com que velocidade cada mercado retorna ao
equilíbrio; a transmissão é simétrica a altas e baixas; existe uma banda de
não-arbitragem; e quais mercados lideram o processo de ajuste.

## Dados e fontes

| Dado | Fonte | Período | Acesso |
|---|---|---|---|
| Preço do leite ao produtor (R$/litro; bruto e líquido, mín/méd/máx), mensal, por região | CEPEA/ESALQ-USP — Indicador do Leite | jan/2016 – abr/2026 | <https://www.cepea.esalq.usp.br/br/indicador/leite.aspx>; planilhas originais em `dados/brutos_cepea/` |
| IPCA, número-índice (deflator) | IBGE, via API OData do IPEA (série `PRECOS12_IPCA12`) | — | `http://www.ipeadata.gov.br/api/odata4/`, baixado pelo script e guardado em `cache_ipea/` (não versionado) |

A base tratada (`dados/cepea_leite_regioes.csv`) reúne 11 regiões: Média
Brasil, BA, CE, ES, GO, MG, MS, PR, RJ, SC e SP. A análise de transmissão usa
as 9 regiões com cobertura completa no período (Ceará e Mato Grosso do Sul
têm séries mais curtas e ficam fora); meses isolados faltantes em BA, ES e RJ
são interpolados linearmente. A série analisada é o preço líquido médio
(`liq_med`), em termos reais e em logaritmo, com a Média Brasil como mercado
de referência nas análises par a par.

O script complementar `R/analise_preco_leite.R` usa também a série de preço do
leite ao produtor do Paraná disponível no IPEA (`DERAL12_PRLECO12`).

Não há credenciais necessárias: as APIs usadas são públicas.

## Estrutura do repositório

```
.
├── R/
│   ├── construir_base_cepea.R      # lê os .xls do CEPEA -> dados/cepea_leite_regioes.csv
│   ├── analise_transmissao_leite.R # análise de transmissão: figuras, tabelas e relatório
│   ├── gerar_latex.R               # tabelas .tex e cópia das figuras para overleaf/ (não versionado)
│   └── analise_preco_leite.R       # complementar: análise univariada do preço real via API do IPEA
├── dados/
│   ├── brutos_cepea/               # 12 planilhas .xls originais do CEPEA
│   └── cepea_leite_regioes.csv     # base tratada em formato longo (região x mês)
├── resultados_transmissao/
│   ├── fig/                        # fig01_series ... fig07_fevd (PNG)
│   ├── painel_precos.csv           # painel balanceado usado na análise (log do preço real, por região)
│   ├── tab_descritiva.csv          # estatísticas descritivas por região
│   ├── tab_raiz_unitaria.csv       # ADF, PP e KPSS
│   ├── tab_transmissao_par.csv     # cointegração, ECM, ECM assimétrico, M-TAR e Granger, par a par
│   ├── tab_tvecm.csv               # TVECM e teste de Hansen-Seo, par a par
│   ├── tab_fevd.csv                # decomposição da variância (h = 12)
│   ├── resultados.rds              # objetos R das tabelas acima
│   └── relatorio_transmissao.txt   # saída textual dos testes
├── LICENSE
├── LICENSING.md
└── README.md
```

Dicionário de `dados/cepea_leite_regioes.csv`: `regiao` (nome sem acentos),
`date` (primeiro dia do mês), `bruto_min`, `bruto_med`, `bruto_max`,
`liq_min`, `liq_med`, `liq_max` (R$/litro; ausências como `NA`).

O texto do artigo, os PDFs e a pasta `overleaf/` não são versionados (ver
`.gitignore`).

## Como reproduzir

Requisitos: R (>= 4.2) e LibreOffice (`soffice`, usado para converter os `.xls`
do CEPEA). Os pacotes R necessários são instalados pelos scripts quando
ausentes (`tidyverse`, `lubridate`, `zoo`, `urca`, `vars`, `tseries`,
`forecast`, `lmtest`, `sandwich`, `tsDyn`, `reshape2`, `plotly`, `htmlwidgets`,
`httr`, `jsonlite`, `kableExtra`, `readxl`).

A partir da raiz do repositório:

```r
source("R/construir_base_cepea.R")      # opcional: regenera o CSV a partir dos .xls
source("R/analise_transmissao_leite.R") # figuras, tabelas e relatório
source("R/gerar_latex.R")               # tabelas .tex em overleaf/
```

## Licença e citação

Código: MIT ([LICENSE](LICENSE)). Texto, documentação e dados: ver
[LICENSING.md](LICENSING.md). Os preços pertencem ao CEPEA/ESALQ-USP e o IPCA
ao IBGE/IPEA; são redistribuídos aqui apenas para reprodutibilidade acadêmica,
com atribuição. Consulte os termos de uso do CEPEA para outros usos.

> Santos, V. G. B. C. B. (2026). *Transmissão de preços do leite ao produtor entre regiões
> brasileiras: cointegração, correção de erro, assimetria e modelos com limiar (TVECM)*.
> Repositório: https://github.com/valbergregory/transmissao-precos-leite-brasil
