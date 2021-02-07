#Aula4

#### Randomização ######

library(tidyverse)

DF = data.frame(
  AnimalID = 1:20 #Coluna usada como identificador
)

rnorm(10, mean =0, sd = 2) #Gera numeros aleatorios dentro da dist. normal
hist(rnorm(10, mean =0, sd = 2))
hist(runif(1000, mean =0, max = 10))

DF = DF %>%
  mutate(
    Grupo = ifelse(runif(n(), min = 0, max = 1) > 0.5, "Controle", "Tratado") #Ifelse já atribui valores a condições específicas. n() é uma função associada a numeros
  )

controles = sample(DF$AnimalID, nrow(DF) / 2) # Sample pega uma amostra aleatória dos IDs
DF = DF %>%
  mutate(
    Grupo = ifelse(AnimalID %in% controles, "Controle", "Tratado")
  )

#ifelse te permite criar colunas com base em condicionais


DF = data.frame(
  AnimalID = 1:20 #Coluna usada como identificador
  Peso = rnorm(20, 20, 5)
)
#Fazendo várias randomizações e forçando que a randomização escolhida tenha diferença das médias menor que 0.1
for (i in 1:100) {
controles = sample(DF$AnimalID, nrow(DF) / 2)
DF = DF %>%
  mutate(
    Grupo = ifelse(AnimalID %in% controles, "Controle", "Tratado")
  )
  MEDIAS = DF %>% group_by(Grupo) %>%
    summarise(Media = mean(Peso))
  
  peso1 = MEDIAS[1,2]
  peso2 = MEDIAS[2,2]
  
  if (abs(peso1 - peso2) < 0.1) {
    break
  }
}

##### Strings ########

library(tidyverse) #Usa mto o pacote 'stringR'

bases = c("A", "T", "C", "G")

make_random_seq = function(size) {
  #browser() #debug do R
  s = sample(bases, size, replace = T) #Sampling com replace: pode sair A varias vzs, etc...
  s = paste0(s, collapse = "")
  s
}

s1 = make_random_seq(500)

#Procurar substring

alvo = "CCGTA"
str_locate(s1, alvo)
str_locate_all(s1, alvo)

"kleber" %>% str_to_upper()
"kleber" %>% str_to_title()

"kleber foi em casa" %>% str_to_sentence()

str_split("AT CG A AT", " ")

genes = str_split(s1, "AT")

s2 = s1 %>% str_replace_all("T", "U") #Achar um texto es substitui-lo

###### Arquivos e automação ########

#Criando uma pasta para cada envio

ENVIOS = read.csv("caminho para dados_envio.xlsx")

envio = ENVIOS[1,] #Pegando a primeira linha

pasta_base = "Caminho para uma pasta base"
dir.create(paste0(pasta_base, "/A"))

make_folder = function(ID_wanted) {
 envio = ENVIOS %>% filter(ID == ID_wanted)
if (envio$`Arquivo?` == "sim") {
  origem_arquivo = paste0(pasta_base, "/imagem.png")
  destino_arquivo = paste0(pasta_base, "/", envio$ID, "/imagem.png")
  file.copy(origem_arquivo, destino_arquivo)
}

filename = "caminho para doc_base.txt"

arquivo_texto = file(filename)
texto_base = readLines(arquivo_texto)
close(arquivo_texto)


texto_base = texto_base %>% 
  str_replace_all("_COUNTRY_NAME_", envio$Pais) %>%
  str_replace_all("_VALUE_", as.character(envio$Valor))


arquivo_html = paste0(pasta_base, "/", envio$ID, "/texto", envio$Pais, ".html")
arquivo_html = file(nome_arquivo_html)
writeLines(texto_base, arquivo_html)
close(arquivo_texto)
}

map(ENVIOS$ID, make_folder) #Roda a função make_folder pra todos os IDs

###### PDFs ########

#Baixando papers no R por DOI/link

link_paper = "link do paper do kleber" #Vai precisar do scihub. Scihub só adiciona o seu domínio na URL

link_scihub = paste0("https://sci-hub-do/", link_paper)

library(rvest) #Harvest -  é o equivalente ao beautifulSoup do python

page = read_html(link_scihub)

article = page %>% 
  html_nodes("#article") %>%
  html_nodes("iframe") %>%
  html_attr("src")

link_pdf = paste0("https:", article)

download.file(url = link_pdf, destfile = "~/Documents/meu_artigo.pdf", quiet = F)

#Ler artigo PLOS: Best practices for Scientific Computing
#Ler artigo: Good enough practices for Scientific Computing