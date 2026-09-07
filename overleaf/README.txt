PROJETO OVERLEAF — Transmissão de preços do leite (FORMATO ANPEC)
=================================================================

Adaptado às normas do Encontro Nacional de Economia (ANPEC):
  - até 20 páginas (incluindo referências e anexos);
  - papel A4; fonte Times New Roman 12 (pacote mathptmx); espaço simples;
  - margens de 2,5 cm (atende ao mínimo: >=1,5 cm laterais, >=2 cm sup./inf.);
  - 1a página com: título (PT e EN), resumo + abstract, palavras-chave +
    keywords, classificação JEL e indicação da Área ANPEC.

IMPORTANTE — avaliação cega (dois arquivos):
  A ANPEC exige DOIS arquivos: (1) VERSÃO ANÔNIMA, sem qualquer identificação
  de autoria; (2) versão completa com autores. O main.tex já está ANÔNIMO.
  Para a versão identificada, descomente o bloco de autores no main.tex
  (logo após o título) e recompile, gerando o segundo PDF.

A CONFERIR no edital do ano:
  - Número/nome exato da Área ANPEC (usei "Área 11 - Economia Agrícola e do
    Meio Ambiente", a área natural deste tema).
  - Classificação JEL (usei Q13; Q11; C32).
  - Limite de palavras do resumo, se houver.

Estrutura:
  main.tex            -> documento principal no formato ANPEC (pdfLaTeX)
  preview_main.pdf    -> PDF já compilado (preview)
  tabelas/            -> tabelas LaTeX (booktabs, geradas com kableExtra)
  figuras/            -> 7 figuras em PNG (300 dpi)
  referencias.bib     -> bibliografia opcional (o main.tex traz as referências
                         embutidas via thebibliography)

Como usar no Overleaf:
  1. "New Project > Upload Project" e selecione o .zip.
  2. Compilador: pdfLaTeX (Menu > Compiler).
  3. Compile (rode 2x para as referências cruzadas).

Para regenerar tabelas/figuras a partir dos dados:
  R: construir_base_cepea.R -> analise_transmissao_leite.R -> gerar_latex.R
