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
  arange(-uptake_mm) %>% #ordena pela coluna. O - ordena do maior valor para o menor
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
  group_by(Type, Treatment) %>% #agupando por 2 colunas
 summarise(media_uptake = mean(uptake), #e sumarisando eles
           sd_uptake = sd(uptake)) %>%
  rename(Uptake = media_uptake) #Renomeando uma coluna

############

#Como ler um arquivo externo para o R? E como importar um dataframe do R pra um arquivo?

#Comma Separated Values (CSV) - Não precisa ser necessariamente separado por vírgula, na real

#Pacote readr: Muito performático, mais rápido. Especialmente para big data (milhões de colunas/linhas)

#Para abrir o arquivo, vc pode dar o full_path ou relative_path

arquivo_tabela = "/home/gabriel/Dropbox/repos/projeto_bioestatistica/"

dados = read_delim(arquivo_tabela, delim = "\t") # O arquvico é tsv, não csv

#se eu fiz várias modificações e quero salvar o arquivo modificado, uso a função "write"

write_delim(dados, "/home..../células2.csv", delim = ";") #Salvando com ';' como delimitador

#Mudando o working directory do R:

setwd("/home/.../dir")

#Agora, vc pode abrir com caminhos relativos


#Lendo Excel no R - Usa pacotes

install.packages("readxl")

library(readxl)

outra_tabela = read_excel("../../comportamento.xlsx")


#Merging tables

library(tidyverse)

dados_celulas = read_delim('celulas.csv', delim= "\t")
dados_comportamento = read_excel("comportamento.xlsx")

dados_completos = merge(dados_celulas, dados_comportamento,
                        by.x = "Code", by.y = "Nome", all = T) #Como as colunas correspondentes têm nomes diferentes, tem q especificar o x e o y.
#all: escolhe com o que ficar. Dá pra ficar com tudo (all = T), só com o x (all.x = T) ou só com o y.


###############

#Rodando um teste t

DF = dads_celulas %>%
  filter(Structure %in% c("Cerebellum", "Hippocampus")) %>%
  group_by(Structure) %>%
  summarise(media_celulas = mean(Cells, na.rm = T))
  
#OBS: Se tem NA ali, o R não calcula a média. Mas vc pode lidar ativamente com os NAs usando parametros. na.rm = T leva à remoção dos NAs da tabela.
#Outra opção seria filtrar dados que não apresentam NA.

DF = dads_celulas %>%
  filter(Structure %in% c("Cerebellum", "Hippocampus"), 
         !is.na(Cells)) %>% # ! é o NOT do R
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

cells_cb$Cells # É A MESMA COISA DE USAR O PULL

resultado2 = t.teste(cells_cb$Cells, cells_hp$Cells)

#datatable - pacote de datascience mto performático, mas a sintaxe do tidyverse parece ser mais simples
#Sempre lembrar do bioconductor
#Bibliometrix - bibliometria
#Outros pacotes do Hadley - Ex: lidar com datas