#Abelhas gráficos

library("tidyverse")
setwd("~/Dropbox/bis")


### Modificando o arquivo "peso_categorias.csv"
peso <- read.csv("./peso_categorias.csv")

peso <- peso %>% select("Categoria", "Introd..Total", "Retorno.Total")

peso = peso %>% 
  mutate(Freq = round(Retorno.Total / Introd..Total, 2)) %>%
  filter(Categoria != "Total")

### Plotando gráfico de Retorno por peso (categorico)
ggplot(peso) +
  aes(x = Categoria, y = Freq) + 
  geom_col(aes(fill = Categoria)) +
  xlab("Peso") +
  ggtitle("Frequência de retornos por peso") +
  theme(plot.title = element_text(hjust = 0.5))



peso_completo <- read.csv("./peso_retorno.csv", header=TRUE)

subset <- peso_completo %>% 
  select("corrigido", "PESO") %>%
  mutate("corrigido" = ifelse(corrigido == 0, "Sem retorno", "Retorno"))

ggplot(subset) +
  aes(x = corrigido, y = PESO) + 
  geom_boxplot(aes(fill=corrigido)) +
  geom_point(position= "jitter", alpha=3/10, aes(fill=corrigido)) +
  xlab("") +
  ylab("Massa (mg)") +
  ggtitle("Massa vs. Retorno") +
  theme(plot.title = element_text(hjust = 0.5))

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

levels(frequencia$DATA) <- rev(levels(frequencia$DATA)) #inverting the order of the levels in the DATA variable

ggplot(frequencia) +
  aes(x=DATA, y=mean_freq, fill=COR) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = c("yellow", "blue", "white", "green", "red", "grey"))