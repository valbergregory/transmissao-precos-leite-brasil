# Transmissão de Preços do Leite ao Produtor entre Regiões Brasileiras

Análise econométrica da **integração e transmissão espacial** de preços do leite ao
produtor entre as principais regiões do Brasil, com dados mensais do **CEPEA/ESALQ-USP**
(jan/2016–abr/2026) deflacionados pelo **IPCA**. O estudo aplica o arcabouço clássico de
transmissão de preços agrícolas — cointegração, correção de erro, assimetria e modelos
com limiar (TVECM) — e entrega o **esqueleto LaTeX** do artigo no **formato ANPEC**.

![R](https://img.shields.io/badge/R-4.4-276DC3?logo=r) ![LaTeX](https://img.shields.io/badge/LaTeX-ANPEC-008080) ![License](https://img.shields.io/badge/license-MIT-green)

## Resultados

Os resultados numéricos estão nas tabelas (`overleaf/tabelas/*.tex`,
`resultados_transmissao/tab_*.csv`) e nas figuras (`resultados_transmissao/fig/`,
`overleaf/figuras/`), todos gerados pelos scripts em `R/`. A interpretação e a
redação do artigo são do autor e não fazem parte deste repositório.

<p align="center">
  <img src="resultados_transmissao/fig/fig01_series.png" width="48%" alt="Séries de preço real"/>
  <img src="resultados_transmissao/fig/fig04_tvecm_regimes.png" width="48%" alt="TVECM: banda de não-arbitragem"/>
</p>

## Estrutura do repositório

```
.
├── R/                              # scripts (rodar a partir da raiz do repositório)
│   ├── construir_base_cepea.R      #  1. lê os .xls do CEPEA -> dados/cepea_leite_regioes.csv
│   ├── analise_transmissao_leite.R #  2. análise de transmissão (figuras + tabelas + relatório)
│   ├── gerar_latex.R               #  3. gera as tabelas .tex e copia as figuras para overleaf/
│   └── analise_preco_leite.R       #  (complementar) previsão univariada via API do IPEA
├── dados/
│   ├── brutos_cepea/               # 12 planilhas .xls originais do CEPEA (fonte)
│   └── cepea_leite_regioes.csv     # base tratada (long): região × mês × preços
├── resultados_transmissao/
│   ├── fig/                        # 7 figuras (PNG 300 dpi)
│   ├── tab_*.csv                   # tabelas de resultados
│   └── relatorio_transmissao.txt   # saída textual dos testes
├── overleaf/                       # esqueleto LaTeX (formato ANPEC) p/ upload
│   ├── main.tex                    # estrutura + tabelas + figuras; prosa: % AUTHOR WRITES
│   ├── referencias.bib, README.txt
│   └── tabelas/ (booktabs, kableExtra)  e  figuras/
├── LICENSE  ·  .gitignore  ·  README.md
```

PDFs, DOCX e pacotes `.zip` não são versionados (`.gitignore`).

## Como reproduzir

**Requisitos:** R ≥ 4.2 · [LibreOffice](https://www.libreoffice.org/) (converter os `.xls`
do CEPEA) · uma distribuição LaTeX (para compilar o Overleaf localmente, opcional).

Pacotes R: `tidyverse, lubridate, zoo, urca, vars, tseries, forecast, lmtest, sandwich,
tsDyn, reshape2, plotly, htmlwidgets, httr, jsonlite, kableExtra, readxl` (os scripts
instalam o que faltar).

Rode **a partir da raiz do repositório**:

```r
source("R/construir_base_cepea.R")      # (opcional) regenera o CSV a partir dos .xls
source("R/analise_transmissao_leite.R") # gera figuras, tabelas e relatório
source("R/gerar_latex.R")               # gera as tabelas .tex e atualiza overleaf/
```

Para o artigo: suba a pasta `overleaf/` no [Overleaf](https://www.overleaf.com)
(*New Project → Upload Project*), compilador **pdfLaTeX**, e redija o texto nos
pontos marcados com `% AUTHOR WRITES`.

## Dados

- **Preço do leite**: Indicador CEPEA/ESALQ-USP — preço líquido médio recebido pelo
  produtor (R$/litro), mensal, por região. Fonte: <https://www.cepea.esalq.usp.br/br/indicador/leite.aspx>
- **Deflator (IPCA)**: número-índice do IBGE, obtido via **API OData do IPEA**
  (série `PRECOS12_IPCA12`).
- Mantêm-se as 9 regiões com cobertura completa (Média Brasil + BA, ES, GO, MG, PR, RJ,
  SC, SP); Ceará e Mato Grosso do Sul foram excluídos por séries interrompidas.

## Metodologia

Raiz unitária (ADF, PP, KPSS) · cointegração de **Johansen** e **Phillips-Ouliaris** ·
**ECM** e **ECM assimétrico** (von Cramon-Taubadel & Loy) · cointegração com limiar
**M-TAR** (Enders & Siklos) · **TVECM** com teste de **Hansen & Seo** (banda de
não-arbitragem; Balke & Fomby) · **causalidade de Granger** · **VAR/IRF/FEVD**.

## Licença, citação e uso de IA

Code: MIT ([LICENSE](LICENSE)). Text, documentation and data: see [LICENSING.md](LICENSING.md).

Código sob licença **MIT** (ver [LICENSE](LICENSE)). Os dados pertencem ao CEPEA/ESALQ-USP
e ao IBGE/IPEA (redistribuídos apenas para reprodutibilidade acadêmica).

O `overleaf/main.tex` contém apenas estrutura, tabelas e figuras; a prosa do artigo é
redigida integralmente pelo autor fora deste repositório. Ferramentas de IA generativa
foram usadas na organização do código e do repositório; a declaração de uso de IA
exigida pelo evento ou revista será redigida pelo autor no momento da submissão.

> Bezerra, V. (2026). *Transmissão de preços do leite ao produtor entre regiões
> brasileiras: cointegração, correção de erro, assimetria e modelos com limiar (TVECM)*.

**Autor:** Valber Bezerra — valber.gregory@gmail.com
