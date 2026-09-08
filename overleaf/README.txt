PROJETO OVERLEAF — Transmissão de preços do leite (FORMATO ANPEC)
=================================================================

O main.tex é um ESQUELETO: preâmbulo no padrão ANPEC, títulos de seção,
tabelas e figuras geradas pelo código em R e as referências. Toda a prosa
(resumo, abstract, palavras-chave, seções) é redigida pelo autor nos pontos
marcados com "% AUTHOR WRITES". Nenhum texto do artigo é versionado aqui.

Normas ANPEC já refletidas no preâmbulo:
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
  - Número/nome exato da Área ANPEC.
  - Classificação JEL.
  - Limite de palavras do resumo, se houver.
  - Cada entrada da bibliografia (thebibliography e referencias.bib) contra
    o original antes de citar.

Estrutura:
  main.tex            -> esqueleto no formato ANPEC (pdfLaTeX)
  tabelas/            -> tabelas LaTeX (booktabs, geradas com kableExtra)
  figuras/            -> 7 figuras em PNG (300 dpi)
  referencias.bib     -> bibliografia opcional (o main.tex traz as referências
                         embutidas via thebibliography)

Como usar no Overleaf:
  1. "New Project > Upload Project" com um .zip desta pasta.
  2. Compilador: pdfLaTeX (Menu > Compiler).
  3. Compile (rode 2x para as referências cruzadas).

Para regenerar tabelas/figuras a partir dos dados:
  R: construir_base_cepea.R -> analise_transmissao_leite.R -> gerar_latex.R
