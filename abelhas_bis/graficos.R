#Abelhas gráficos

library("tidyverse")
library("rstudioapi")
library("patchwork") #To draw grids and annotate multi-plot figures

setwd(dirname(getActiveDocumentContext()$path))


### Modificando o arquivo "peso_categorias.csv"
peso <- read.csv("./peso_categorias.csv")

peso <- peso %>% select("Categoria", "Introd..Total", "Retorno.Total")

peso = peso %>% 
  mutate(Freq = round(Retorno.Total / Introd..Total, 2)) %>%
  filter(Categoria != "Total")

### Plotando gráfico de Retorno por peso (categorico)
p1A <- ggplot(peso) +
  aes(x = Categoria, y = Freq) + 
  geom_col(aes(fill = Categoria)) +
  xlab("Peso") +
  ggtitle("Frequência de retornos por peso") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
        

p1A


peso_completo <- read.csv("./peso_retorno.csv", header=TRUE)

subset <- peso_completo %>% 
  select("corrigido", "PESO") %>%
  mutate("corrigido" = ifelse(corrigido == 0, "Sem retorno", "Retorno"))

p1B <- ggplot(subset) +
  aes(x = corrigido, y = PESO, color=corrigido) + 
  geom_boxplot() +
  geom_point(position= "jitter", alpha=3/10) +
  xlab("") +
  ylab("Massa (mg)") +
  ggtitle("Massa vs. Retorno") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none')

p1B

frequencia <- peso_completo %>%
  select(COR, DATA, corrigido) %>% #Só usarei as colunas que importam
  mutate("corrigido" = ifelse(corrigido == 0, "Sem retorno", "Retorno")) %>%
  group_by(COR, DATA) %>% #Agrupa por COR e DATA
  mutate(total = n()) %>% #N total de abelhas por cor/data
  group_by(corrigido, .add=TRUE) %>% #Agrupa por cor, data e corrigido (se houve retorno ou não)
  mutate(relative = n()) %>% #N de abelhas por cor/data/corrigido
  ungroup() %>% #No more need for groupings for now
  mutate(freq = relative / total) %>% #calculando frequencia por meio das duas colunas criadas
  filter(corrigido == "Retorno") %>% #Mantendo apenas os retornos
  select(!c(total, relative, corrigido)) %>% #Removendo colunas não essenciais (não são necessarias, já que a frequencia sumariza ambas)
  group_by(COR,DATA) %>%
  summarise(mean_freq = mean(freq)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor) #Changing character columns to factors

#In order to plot in a specific order, we need to change the order of the levels of our categorical variables (factors): COR e DATA
levels(frequencia$COR) <- c("AMARELO", "AZUL", "BRANCO", "VERDE", "VERMELHO", "CONTROLE") 

#levels(frequencia$DATA) <- rev(levels(frequencia$DATA)) #inverting directly the order of the levels in the DATA variable
#frequencia$DATA <- ordered(frequencia$DATA, levels = c("26/05/2020", "16/07/2020", "14/10/2020")) #Manually ordering the levels of the DATA factor
frequencia$DATA <- factor(frequencia$DATA, levels = c("26/05/2020", "16/07/2020", "14/10/2020"), ordered =TRUE) #Manually ordering the levels of the DATA factor

p1C <- ggplot(frequencia) +
  aes(x=DATA, y=mean_freq, fill=COR) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = c("yellow", "blue", "white", "green", "red", "grey"))

p1C


## Manipulating the grid with patchwork to generate multi-plot figures

layout <- "
AB
CC
"

p1A + labs(title = element_blank()) + 
  p1B + labs(title = element_blank()) + 
  p1C + theme(legend.position = "right") +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')



## Plotting an inset element (a plot inside another plot) with patchwork

p1B_transparent =  p1B +
  labs(title = element_blank()) + 
  theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", colour = NA))

p1B_transparent

p1C + ylim(0, 2) + #Had to increase ylim to fit the inset
inset_element(p1B_transparent, .5,.5,1,1)
