#Aula3 - Visualização de dados no ggplot2

#Rstudio possui diversas cheatsheets. Bom dar uma olhada
#Em especial, olhar cheatsheet do ggplot2 no site do Rstudio
#ggplot2 - grammar graphics - gramática de gráficos
#Incorpora fundamentos de como fazer uma sintaxe limpa
#Em especial, a teoria de que graficos mapeiam propriedades estéticas em dados

library(tidyverse) #Carregando o tidyverse (e consequentemente o ggplot2)

#Apesar do nome do pacote ser ggplot2, a função chamada é sempre a 'ggplot'
#Deprecated: não é recomendado. Não é mais suportado. Seu código pode se tornar obsoleto rápido.

DF = ChickWeight
summary(ChickWeight)

#O ggplot é escrito em camadas. Vc vai sempre adicionando novas coisas
# aes (aesthetic) - É o mapeamento estético. Nele vc diz qual variavel está associada esteticamente (visualmente) a qual eixo, p. ex.
#geom - objeto geométrico
ggplot(DF) +
  aes(x = weight) +
  geom_histogram() #Como é um histograma, ele conta automaticamente o peso das galinhas
 # Esse é um exemplo de plot básico. Dá pra adicionar mais coisase em novas camadas 

ggplot(DF) +
  aes(x = weight) +
  geom_histogram() + #Como é um histograma, ele conta automaticamente o peso das galinhas
  labs(title = "Pesos das galinhas", subtitle = "Esse foi o primeiro gráfico", x = "Esse é o peso em gramas", y = "Frequência")

#Há quatro tipos de dietas
summary(DF$Diet)

#Vamos plotar os dados dessas dietas
#Alpha é algo que está sempre associado a transparencia de imagem em computadores
#Fazendo plot de pontos
ggplot(DF) +
  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
  geom_point(size=2, alpha = 0.2, position = position_jitter(width = 0.2)) #Com pontos, não fica tão visível. O jitter espalha melhor os pontos pra ficar mais viśivel

#Gráfico de barras (obs: o geom_col automaticamente soma valores)
ggplot(DF) +
  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
  geom_bar(stat = "summary") #O padrão do 'summary' é fazer a média

#Fazendo um geoom_boxplot
ggplot(DF) +
  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
  geom_boxplot() 

#Violin plot
ggplot(DF) +
  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
  geom_violin() 


#Tbm podemos sobrepor plots: o que é botado primeiro aparece por baixo
ggplot(DF) +
  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
  geom_boxplot() +
  geom_point(size=2, alpha = 0.2, position = position_jitter(width = 0.2))

#Obs: Vc sempre pode fazer mapeamentos estéticos diferentes para diferentes geoms
#ggplot(DF) +
#  aes(x = Diet, y = weight) + #Fazendo o mapeamento estético
#  geom_point() +
#  geom_segment( #Usado para traças linhas/segmentos no gráfico (e.g. linha que repesente a média)
#    aes(
#      x = Diet -0.1, y = weight, xend = Diet + 0.1, yend = weight
#    )
#  )


#Usando geom_line para representar linhas para uma única galinha
ggplot(DF %>% filter(Chick == 1)) +
  aes(x = Time, y = weight) +
  geom_line() +
  geom_point()

#Gerando o gráfico para cada galinha (agrupando por Chick, para as 4 primeiras galinhas)
ggplot(DF %>% filter(Chick <4)) + 
  aes(x = Time, y = weight, group = Chick, color = Diet) +
  geom_line() +
  geom_point()

###########

DF = anscombe
coeficiente = cor(DF$x1, DF$y1) #Calculando coeficiente de correlação

ggplot(DF) +
  aes(x = x1, y = y1, size = x1, label = x1) + # O size aumenta o tamanho do ponto de acordo com o valor
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = F, color = "red", size = 3) + # geom_smooth dá a regressão da reta
  geom_text(position = position_nudge(y = 1)) + #Adicionando os labels correspondentes aos valores de x. O nudge empurra o texto dos labels. Precisa ter o label definido 
  scale_size_continuous(range = c(3,12)) + # Muda a escala do tamanho dos elementos visuais. Serve para aumentar bastante os tamanhos
  annotate("text", x = 12, y = 6, label = paste0("r = ", signif(coeficiente,3), size = 6)) #Plota o coeficiente de correlação no gráfico. Annotate adiciona algum (qqr) texto no gráfico

#paste() cola adicionando espaço
#paste0() cola sem adicionar espaço

#OBS: Correlação é algo que não te dá uma boa visão do comportamento dos dados. Sempre plote os dados antes de confiar copletamente em sumários estatísticos

DF = anscombe

#OBS: Há vários pacotes em cima do ggplot para fazer inúmeras outras coisas: Nuvem de palavras, etc. Mas sempre mantém o lance da gramatica, de mapear dados a elementos visuais
#Reespecificando os dados para diferentes geoms (objetos genométricos)
#Nesse caso, vc pode representar dados que obedecem diferentes filtros/critérios de forma diferente no gráfico
#Vc pode fazer coisas com subsets dos seus dados
ggplot(DF) +
  aes(x = x1, y = y1) +
  geom_point(data = DF %>% filter(x1 < 10), size = 3) +
  geom_point(data = DF %>% filter(x1 >= 10), size = 3, color = "blue") 
  
#######################

DF = ChickWeight

ggplot(DF %>% filter(Chick <4)) + 
  aes(x = Time, y = weight, group = Chick) +
  geom_line() +
  geom_point() +
  facet_wrap(~Diet) #Quebra os gráficos em diferentes gráficos para as diferentes dietas. Dá pra fazer com múltiplas variáveis

#Diferentes pacotes adicionam diferentes funções/geoms

ggplot(DF %>% filter(Chick <4)) + 
  aes(x = Time, y = weight, group = Chick) +
  geom_line() +
  geom_point() +
  facet_wrap(~Diet) + #Quebra os gráficos em diferentes gráficos para as diferentes dietas. Dá pra fazer com múltiplas variáveis
  scale_y_continuous(trans = "log", breaks = c(50, 150, 400)) #Convertendo para log a escala. Tbm adiciona os ticks em locais específicos
  scale_y_continuous(trans = "log", breaks = scales::pretty_breaks(n = 7)) #Usando a função pretty_breaks do pacote scales para definir os breaks automaticamente

#Tem um pacte (scales), que possui um função q permite escolher os breaks automaticamente pra vc


ggplot(DF) + 
  aes(x = Time, y = weight, group = Chick, color = Diet) +
  geom_line() +
  geom_point() +
  labs(title = "Nome do gráfico", x = "Tempo", y = "Peso") +
  theme(legend.position = "bottom") + #Dá pra mudar onde os eixos aparecem tbm
  theme_minimal()

#OBS: Uma vantagem do gráfico do ggplot é q o gŕafico já está pronto. É só mandar refazer com outros dados, adaptando ou os dados ou o código...

#Vc pode criar o seu próprio tema, partindo a partir de um tema preexistente. Um tema e um conjunto de propriedades específicas

theme_kleber = theme_minimal() +
  theme(legend.position = "bottom",
       plot.background = element_rect(fill = "blue") )

ggplot(DF) + 
  aes(x = Time, y = weight, group = Chick, color = Diet) +
  geom_line() +
  geom_point() +
  labs(title = "Nome do gráfico", x = "Tempo", y = "Peso") +
  theme(legend.position = "bottom") + #Dá pra mudar onde os eixos aparecem tbm
  theme_kleber

#Cores no R:
#Vc pode adicionar como string ou HEX (hexadecimal)

library(RColorBrewer) # Dá diversas paletas pro ggplot. Te ajuda a escolher as cores para, por exemplo, pessoas daltônicas

ggplot(DF) + 
  aes(x = Time, y = weight, group = Chick, color = Diet) +
  geom_line() +
  geom_point() +
  labs(title = "Nome do gráfico", x = "Tempo", y = "Peso") +
  theme(legend.position = "bottom") + #Dá pra mudar onde os eixos aparecem tbm
  scale_color_brewer()

#####################

#power.t.test -> é uma função que recebe valores e calcula o que vc não especificar (seja o poder, o n, etc...)

calcula_n = function (sample_size) {
  resultado_poder = power.t.test(n = sample_size, delta = 0.5, sig.level = 0.05, power = NULL)
  resultado_poder$power
}

#Retornando a curva de poder inteira (para sample_size de 5 até 100, por exemplo)

poderes = numeric(100) #Cria um array numérico de 100 elementos, todos eles com 0.

for (n in 5:100) {
  poderes[n] = calcula_n(n) #Calcula o n para aquele valor amostral e o guarda na posição correspondente no vetor poderes
}

#Agora temos a curva de poder dentro de um vetor
poderes[10] #Retorna poder para sample_size de 10
poderes[25] #Retorna poder para sample_size de 25
poderes[95] #Retorna poder para sample_size de 95


#mas o for é mto lento em R e é desencorajado.
#usa o map então. Ele está dentro do purr
#O map aplica uma função a cada um dos elementos de um vetor que vc passa para ele. Mto mais rápido que o for

#Para além disso, o dataframe é melhor para plotar, então vamos fazer o cacula_n retornar um dataframe com duas colunas: N e Poder

calcula_n = function (sample_size) {
  resultado_poder = power.t.test(n = sample_size, delta = 0.5, sig.level = 0.05, power = NULL)
  return (data.frame(
    N = sample_size,
    Poder = resultado_poder$power
  ))
}

calcula_n(10)

poderes = map_dfr(5:100, calcula_n) #map_dfr pega um vetor (no caso, os possíveis sample_sizes) e aplica uma função a cada elemento (no caso, a função 'calcula_n') e retorna um dataframe. dfr = dataframe_rows

ggplot(poderes) +
  aes(x = N, y = Poder) +
  geom_line() +
  #geom_point() +
  geom_hline(yintercept = 0.8, linetype = 'dashed', color = 'red') #Linha horizontal passando por onde o poder = 80%

poderes %>% filter(Poder > 0.6) %>% slice(1) %>% pull(N) # Mostra o valor de sample size a partir do qual o poder é maior q 60%. O slice te retorna só o primeiro valor.


#O slice é compatível com slicing notation. Ele pode retornar os primeiros 3 valores, por exemplo.

poderes %>% filter(Poder > 0.6) %>% slice(1:3) %>% pull(N) 

#Outra possibilidade com o for (para gerar um dataframe) é usar o rbind (rowbind)

poderes = data.frame()

for (n in 5:100) {
 poderes = rbind(poderes, calcula_n(n))
}


###############################################
#EXERCICIOS

#Plot1

ggplot(chickwts) +
  aes(x = feed, y = weight) +
  geom_point()


#Plot2

#Primeira resposta (não consegui adicionar a legenda)
weight_mean = mean(chickwts$weight)
  
ggplot(chickwts) +
  aes(x = feed, y = weight) +
  geom_point(data = chickwts %>% 
               filter(weight <= weight_mean), 
             color = "blue", 
             alpha = 0.5) +
  geom_point(data = chickwts %>% 
               filter(weight > weight_mean), 
             color = "red", 
             alpha = 0.5) +
  geom_hline(yintercept = weight_mean, linetype = 'dashed') +
  theme_minimal()


# Segunda resposta: deu mais trabalho, mas eu consegui

weight_mean = mean(chickwts$weight)

new_chickwts = chickwts %>%
  mutate(avg_stats = case_when(
    weight < weight_mean ~ "Below average",
    weight >= weight_mean ~ "Equal or above average"
  ))
  
ggplot(new_chickwts) +
  geom_point(aes(x = feed, y = weight, color = avg_stats), alpha = .5) +
  geom_hline(yintercept = weight_mean, linetype = 'dashed') +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(
    values = c("blue", "red")) +
  scale_alpha_manual(values = c(.5, .5))
  

#Plot3

#Foi o que eu conegui fazer por mim msm:
new_chickwts = chickwts %>%
  group_by(feed) %>%
  mutate(means = mean(weight))

ggplot(new_chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  geom_hline(yintercept = new_chickwts$means) +
  theme_minimal() +
  theme(legend.position = "none")


#Resposta do Kleber:

ggplot(chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) +
  theme_minimal() +
  theme(legend.position = "none")

#Messing around - And avoiding deprecated options

ggplot(chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar", width = 0.5) +
  theme_minimal() +
  theme(legend.position = "none")


#Não precisa do fun.min and fun.max, aparentemente
ggplot(chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  stat_summary(fun = mean, geom = "crossbar", width = 0.8) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = .5) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(color = "Mean")


ggplot(chickwts) +
  aes(x = feed, y = weight, color = feed) +
  geom_point() +
  stat_summary(fun = mean, geom = "crossbar", width = .8, alpha = .5) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(color = "Mean")


##Plot4

ggplot(iris) +
  aes(x = Petal.Length, y = Petal.Width, 
      color = Species) +
  #theme_minimal() +
  theme(legend.position = 'bottom') +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~Species, scales = "free")


##Plot5

##Almost got it with facet_grid
ggplot(iris) +
  aes(x = Petal.Length, y = Petal.Width, 
      color = Species) +
  #theme_minimal() +
  theme(legend.position = 'bottom') +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~Species, scales = "free", margins = T)

##Olhei o do Kleber

#Creating a dataframe with a duplicated dataframe: one untouched, one where the contents of columns 'Species' has been changed to 'ALL'

DF = rbind(iris,
           iris %>%
             mutate(Species = "ALL"))

ggplot(DF) +
  aes(x = Petal.Length, y = Petal.Width, 
      color = Species) +
  #theme_minimal() +
  theme(legend.position = 'bottom') +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~Species, ncol = 2, scales = "free")

##Plot6

#Tive que dar uma olhada pra entender o q fazer (converter os nomes das colunas em uma coluna do dataframe)

DF = mtcars %>%
  select(mpg) %>%
  mutate(modelos = row.names(mtcars))

ggplot(DF) +
  aes(x = mpg, y = modelos) +
  geom_text(aes(label = modelos), 
            position = position_nudge(x = .5),
            size = 3, check_overlap = T, hjust = 0) +
  xlim(10,37) +
  geom_point() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


##Plot7

##Ordena pela coluna numerica e gera um vetor com a variável categorica. Daí, vc vai ter seu plot ordenado
##Tbm tem como ordenar da forma que vc quiser
## Exemplos aqui: https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html

DF = mtcars %>%
  select(mpg) %>%
  arrange(mpg) %>%
  mutate(modelos = row.names(mtcars)) %>%
  mutate(modelos = factor(modelos, levels = modelos))

ggplot(DF) +
  aes(x = mpg, y = modelos) +
  geom_text(aes(label = modelos), 
            position = position_nudge(x = .5),
            size = 3, check_overlap = T, hjust = 0) +
  xlim(10,37) +
  geom_point() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
