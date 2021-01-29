#Aula3 - Visualização de dados no ggplot2

#Olhar cheatsheet do ggplot2 no site do Rstudio
#ggplot2 - grammar 
#Incorpora fundamentos de como fazer uma sintaxe limpa 

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

DF = ascombe
coeficiente = cor(DF$x1, DF$y1) #Calculando coeficiente de correlação

ggplot(DF) +
  aes(x = x1, y = y1, size = x1) + # O size aumenta o tamanho do ponto de acordo com o valor
  geom_ponint(color = "green") +
  geom_smooth(method = "lm", se = F, color = "red", size = 3) + # geom_smooth dá a regressão da reta
  geom_text(position = position_nudge(y = 1)) + #Adicionando os labels correspondentes aos valores de x. O nudge empurra o texto dos labels 
  scale_size_continuous(range = c(3,12)) + # Muda a escala do tamanho dos elementos visuais. Serve para aumentar bastante os tamanhos
  annotate("text", x = 12, y = 6, label = paste0("r = ", signif(coeficiente,3), size = 6)) #Plota o coeficiente de correlação no gráfico

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

calcular_n = function (sample_size) {
  resultado_poder = power.t.test(n = sample_size, delta = 0.5, sign.level = 0.05, power = NULL)
  resultado_poder$power
}

poderes = numeric(100)

###Rever como ele fez isso na aula
poderes = data.frame()
for (n in 5:100) {
 rbind(poderes(calcular_n(n)))
}

#mas o for é mto lento em R e é desencorajado
#usa o map então

calcular_n = function (sample_size) {
  resultado_poder = power.t.test(n = sample_size, delta = 0.5, sign.level = 0.05, power = NULL)
  return (data.frame(
    N = sample_size,
    Poder = resultado_poder$power
  ))
}

calcula_n(10)

poderes = map_dfr(5:100, calcula_n)

ggplot(poderes) +
  aes(x = N, y = Poder) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = 'dashed', color = 'gray')

poderes %>% filter(Poder > 0.6) %>% slice(1) %>% pull(N)