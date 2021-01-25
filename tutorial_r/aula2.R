# Aula 2

#Cross-validated - Stack exchange de estatística

#Dataframe: Basicamente ele é uma lista de vetores. Uma lista de colunas. Cada coluna vai ter um tipo. OBS: NA pode ser adicionado em qqr coluna

View(mtcars) #vendo o dataset do R

#Criando dataframe

D = data.frame(
  altura = c(1.92, 1.68, 1.20),
  peso = c(100, NA, 30), #NA = NOT AVAILABLE
  codigo = c("A", "B", "C"),
  categorias = factor(c("A", "B", "C")), # Adicionando isso, vc diz explicitamente que esse vetor deve ser considerado como factor
  stringsAsFactors = FALSE # Se não botar isso, considera que a coluna de texto é um fator
  # Se vc não fala nada, o R interpreta qqr coluna com strings como categorias. A partir do R v4.0 não precisa mais preocupar com isso
)

View(D)

#Usando funções de outros pacotes/instalando pacotes

install.packages("tidyverse") # instalando tidyverse

library("tidyverse") # carregando o tidyverse

#tidyverse: conjunto de pacotes criado pelo Hadley Wickham, possivelmente o maior programador em R. Recomendação de livro: R for data science. Os pacotes funcionam mto bem juntos

#dplyr: ajuda na manipulação de tabelas - usa o pipe %>%: lê-se como "pipes to"

#Pegando carros mais eficientes

carros_eficientes = mtcars %>% filter(mpg > 30)

View(carros_eficientes)

#Fazendo múltiplos pipes. Os pipes te permitem encadear muuuitas operações de uma vez só. P.S. A ordem das operações importa

carros_eficientes = mtcars %>% 
  filter(mpg > 30) %>%
  summarise(media_hp = mean(hp))

View(carros_eficientes)

#dplyr tem múltiplos verbos: summarise, filter, mutate (para adicionar colunas), select (selecionar colunas), rename (renomear colunas), arrange (ordena seus dados), groupby (ordena por grupos)

#Usando um novo dataset do R: CO2

View(CO2)

DF = CO2

DF2 = DF %>%
  filter(Type == "Quebec", Treatment == "chilled") %>%
  mutate(uptake_mm = uptake / 1000) %>% #Obs: mutate escreve por cima se já tiver uma coluna com aquele nome
  select(Plant, Type, Treatment, uptake_mm) %>% # O select seleciona as colunas na ordem que vc quiser.
  select(-Type, -Plant) %>% # Removendo colunas que vc não quer...
  arrange(-uptake_mm) %>% #ordena pela coluna. O - ordena do maior valor para o menor
  slice(1:5) # Pegando apenas os 5 maiores valores

DF_Quebec_Chilled = DF %>%
  filter(Type == 'Quebec', Treatment == "chilled") %>%
  summarise(media_uptake = mean(uptake))

DF_Quebec_NonChilled = DF %>%
  filter(Type == 'Quebec', Treatment == "nonchilled") %>%
  summarise(media_uptake = mean(uptake))

DF_Final = rbind(DF_Quebec_Chilled, DF_Quebec_NonChilled)

# Mas as operações para gerar as duas DFs acimas são mto similares. Deve ter como fazer tudo de uma vez, certo?
# Podemos usar o group_by

DF_Medias = DF %>%
  group_by(Type, Treatment) %>% #agrupando por 2 colunas
 summarise(media_uptake = mean(uptake), #e sumarisando eles
           sd_uptake = sd(uptake)) %>%
  rename(Uptake = media_uptake) #Renomeando uma coluna

############

#Como ler um arquivo externo para o R? E como importar um dataframe do R pra um arquivo?

#Comma Separated Values (CSV) - Não precisa ser necessariamente separado por vírgula, na real

#Pacote readr: Muito performático, mais rápido. Especialmente para big data (milhões de colunas/linhas)

#Para abrir o arquivo, vc pode dar o full_path ou relative_path

arquivo_tabela = "./tutorial_r/dados/celulas.csv"

dados = read_delim(arquivo_tabela, delim = "\t") # O arquivo é tsv, não csv

#se eu fiz várias modificações e quero salvar o arquivo modificado, uso a função "write"

write_delim(dados, "tutorial_r/dados/células2.csv", delim = ";") #Salvando com ';' como delimitador

#Mudando o working directory do R:
setwd("tutorial_r/dados/")
#Agora, vc pode abrir com caminhos relativos


#Lendo Excel no R - Usa pacotes

install.packages("readxl")

library(readxl)

outra_tabela = read_excel("comportamento.xlsx")


#Merging tables

library(tidyverse)

dados_celulas = read_delim('celulas.csv', delim= "\t")
dados_comportamento = read_excel("comportamento.xlsx")

dados_completos = merge(dados_celulas, dados_comportamento,
                        by.x = "Code", by.y = "Nome", all = T) #Como as colunas correspondentes têm nomes diferentes, tem q especificar o x e o y.
#all: escolhe com o que ficar. Dá pra ficar com tudo (all = T), só com o x (all.x = T) ou só com o y.


###############

#Rodando um teste t

DF = dados_celulas %>%
  filter(Structure %in% c("Cerebellum", "Hippocampus")) %>% # somente linhas onde a coluna 'Structure' tem como valores "Cerebellum" e "Hippocampus" 
  group_by(Structure) %>%
  summarise(media_celulas = mean(Cells, na.rm = T))
  
#OBS: Se tem NA ali, o R não calcula a média. Mas vc pode lidar ativamente com os NAs usando parametros. na.rm = T leva à remoção dos NAs da tabela.
#Outra opção seria filtrar dados que não apresentam NA.

DF = dados_celulas %>%
  filter(Structure %in% c("Cerebellum", "Hippocampus"), 
         !is.na(Cells)) %>% # ! é o NOT do R. No caso, remove linhas que têm NA na coluna 'Cells'
  group_by(Structure) %>%
  summarise(media_celulas = mean(Cells, na.rm = T))

#Fazendo teste t com o R - precisa de dois vetores

cells_cb = dados_celulas %>%
  filter(Structure == "Cerebellum", !is.na(Cells)) %>%
  pull(Cells) # Ele nao seleciona a coluna Cells. Ele puxa o VETOR. É um conjunto de números, somente.
  
cells_hp = dados_celulas %>%
  filter(Structure == "Hippocampus", !is.na(Cells)) %>%
  pull(Cells) # Ele nao seleciona a coluna Cells. Ele puxa o VETOR. É um conjunto de números, somente.

resultado_teste = t.test(cells_cb, cells_hp) #Resultado do Teste

#O resultado do t.test dá uma lista com listas dentro. Vc pode manipular isso dentro do R.

#Pegando atributos do resultado

resultado$p.value
resultado$conf.level


## ALTERNATIVAMENTE, pegar o atributo te retorna o vetor direto, então dá pra fazer isso em menos linhas de códigos.

dados_celulas$Cells # É A MESMA COISA DE USAR O PULL

resultado2 = t.test(dados_celulas$Cells, dados_celulas$Cells)

#datatable - pacote de datascience mto performático, mas a sintaxe do tidyverse parece ser mais simples
#Sempre lembrar do bioconductor
#Bibliometrix - bibliometria
#Outros pacotes do Hadley - Ex: lidar com datas



#######################################


#EXERCÍCIOS

#Os exercícios abaixo usam tabelas disponíveis na pasta dados (A-E). Todas as tabelas contém os mesmos dados, mas cada uma em um formato diferente. O desafio é conseguir fazer com que o R leia elas no formato correto. Caso uma dessas tabelas seja carregada com sucesso, ela deve possuir 6 observações, em 2 colunas. O código abaixo serve de teste (todas as linhas devem imprimir TRUE se estiver tudo certo):

library('tidyverse')
library('readxl')

getwd()
setwd("tutorial_r/dados")
  
dados = read_delim("tabelaA.csv", delim = ',') # SEU CÓDIGO PRA LER A TABELA
dados = read_delim("tabelaB.csv", delim = ';')
dados = read_delim("tabelaC.tsv", delim = '\t')
dados = read_excel("tabelaD.xlsx")
dados = read_excel("tabelaE.xlsx", sheet = 2)

# número de linhas e colunas: 6 e 2
nrow(dados) == 6; ncol(dados) == 2

# coluna X é numérica, coluna Y é texto/string/caracteres
class(dados$X) == "numeric"; class(dados$Y) == "character"

# média da coluna *X* é 30
mean(dados$X, na.rm = T) == 30

# quantidade de NAs na coluna *Y* é 1
sum(is.na(dados$Y)) == 1

# 1) Arquivo: tabelaA.csv
# 2) Arquivo: tabelaB.csv
# 3) Arquivo: tabelaC.tsv
# 4) Arquivo: tabelaD.xlsx
# 5) Arquivo: tabelaE.xlsx

# Notem que no Windows, os caminhos são especificados com barra invertida (e.g. “Users\Kleber\Documentos”). O R usa barras normais (/), então cuidado na hora de copiar caminhos.


######################################


#Os exercícios abaixo usam os verbos comuns de dplyr pra manipulação de dados.

#DATASET: mtcars (R base)

library(tidyverse)
View(mtcars)

# • Imprima na tela a coluna hp. Dica: use a função select().

mtcars %>% select(hp)

# • Imprima na tela todas as colunas, exceto a coluna hp.

mtcars %>% select(-hp)

# • Crie um novo data frame mtcars2 que contém só as colunas hp, drat e wt. Dica: a função select() aceita vetores numéricos (e.g. select(5:6) seleciona da 5ª à 6ª coluna)

mtcars2 <- mtcars %>% select(c(hp, drat, wt))
mtcars2 <- mtcars %>% select(4:6)

# • Crie as seguintes colunas em mtcars: kpg (kilômetros por galão - use 1 milha = 1.6 km), kpl (kilômetros por litro - use 1 galão = 3.8 litros). Dica: use a função mutate(). Tente fazer escrevendo mutate() uma única vez.

mtcars %>%
  mutate(kpg = mpg * 1.6, kpl = mpg * 1.6 / 3.8 )

# • Crie um novo data frame mtcars3 que só tem os carros que tem mpg > 20. Dica: use a função filter().


# • Crie um novo data frame mtcars4 que só tem os carros que tem mpg > 20 ou hp > 150. Dica: em R, a barra vertical (|) significa "ou" (o E comercial (&) significa "e", mas não precisa dele aqui).








#DATASET: msleep.csv (disponível na pasta)

# • Carregue os dados da tabela "msleep.csv" num data frame chamado msleep.


# • Crie um data frame msleep_primates contendo apenas os primatas.


# • Faça o data frame conter apenas a coluna sleep_total dos primatas. Escreva isso como continuação da linha de cima.


# • Escreva uma linha que imprima na tela o número de primatas. Dica: use a função nrow().


# • Crie uma variável sono_medio que contenha a duração média do sono de primatas.


# • Calcule a média de sleep_total e a média de brainwt para todas as outras ordens (coluna order), excluindo primatas, e junte tudo em um data frame. Dica: use as funções group_by() e summarise().