# projeto_bioestatistica
Repo para o meu projeto da disciplina 'Estatística e Reprodutibilidade em Ciências da Vida'

Em muitos organismos, há o chamado viés de códon (**codon bias**), que é a preferência de alguns codons específicos em detrimento de outros que codifiquem para o mesmo aminoácido. Esse fenômeno ocorre com muita frequência no genoma mitocondrial.

Colocar algo sobre isso estar associado à eficiência translacional.

Assim sendo, neste projeto iremos avaliar se os códons com mais AT são de fato mais comuns que os com maior porcentagem de GC. Para isso, iremos usar uma amostragem de XX mitogenomas de primatas.

## Pipeline

### Obtenção dos dados

Os dados foram obtidos do genbank por meio do [portal RefSeq de organelas do NCBI](https://www.ncbi.nlm.nih.gov/genome/browse#!/organelles/) usando um [script python](https://github.com/gavieira/python_bioinfo/blob/master/mitodownloader.py). Com isso foram obtidos os arquivos genbank das sequências mitocondriais analisadas.

### Análise dos arquivos gb

Os arquivos genbank foram submetidos ao programa [tRNAscan-SE](https://github.com/UCSC-LoweLab/tRNAscan-SE) para identificar quais os anticodons presentes nos tRNAs mitocondriais.

Além disso, com o auxílio de bibliotecas Python como o [Pandas](https://pandas.pydata.org/) e o [Biopython](https://biopython.org/), as métricas de uso de codon (**codon usage**) foram calculadas para todos os organismos e exportadas para uma tabela. Todas as manipulações podem ser visualizadas [neste jupyter notebook]().
