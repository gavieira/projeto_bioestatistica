# Análise tabela primatas


library("tidyverse")
library("readxl")

#Lendo tabela
setwd("./apresentacao_fabiana")
dados <- readxl::read_excel("../primates/Primates_codon_usage.xlsx")

#Mudando título de coluna
dados <- dados %>% rename(codon_count = "Number of Codon Occurences")

print.data.frame(head(dados, 2))


# Agrupando dados por aa e codon, e somando a ocorrência total de cada códon (soma das 199 spp.)
all_aa = dados %>%
  group_by(Aminoacid, Codon) %>%
  summarise(observed_values = sum(codon_count), .groups = "drop")


#Criando dataframe para acomodar resultados do chisquare
chi_results = data.frame()

##Fazendo um teste qui-quadrado para cada aminoácido
for (aa in unique(all_aa$Aminoacid)) { 
  
  subset = all_aa %>% filter(Aminoacid == aa)
  rows = nrow(subset)
  chi_sqr = chisq.test(subset$observed_values, p = rep(1/rows, rows))
  #chi_sqr = chisq.test(subset$observed_values, p = rep(1/rows, rows), correct = FALSE)
  print(paste("Valor de p para aminoacido", aa, ":", chi_sqr$p.value))
  
  for (i in 1:rows) {
    chi_results = rbind(chi_results, c(
      subset$Aminoacid[i],
      subset$Codon[i],
      chi_sqr$observed[i],
      chi_sqr$expected[i],
      chi_sqr$p.value
    ))
  }
  colnames(chi_results) =  c("Aminoacid", "Codon", "Observed", "Expected", "p-value" )
}

#Salvando teste chisquare em arquivo
write.csv(chi_results, "./chi_sqr_results.csv")


### Contando o número de anticodons
anticodon_count = table(dados$Anticodon)

anticodon_count_df = data.frame(anticodon_count) %>% 
  rename(Anticodon = "Var1")

write.csv(anticodon_count_df, "./anticodons_count.csv", row.names=FALSE)


#Visualizando os códons por aminoácido


ggplot(dados %>% filter(Aminoacid != "End")) +
  aes(x = Codon, y = Fraction) + 
  geom_boxplot() +
  geom_point(size=2, alpha = 0.2, position = position_jitter(width = 0.2)) +
  facet_wrap(~Aminoacid, scales = "free")


## Visualizando códons que batem completamente com anticodon mitocondrial vs. os que não batem

#Criando coluna que separe os grupos
dados <- dados %>%
  mutate(WCstatus = case_when(
    is.na(Anticodon) ~ "noWC",
    TRUE ~ "WC" ))

#Plotando com todos os dados
ggplot(dados %>% filter(Aminoacid != "End")) + #Filtragem para retirar stop codons
  aes(x = WCstatus, y = codon_count) + 
  geom_boxplot() +
  geom_point(size=2, alpha = 0.2, position = position_jitter(width = 0.2), aes(colour = factor(WCstatus))) +
  ggtitle("All data codon count") +
  theme(plot.title = element_text(hjust = 0.5))

                     
ggplot(dados %>% filter(Aminoacid != "End")) + #Filtragem para retirar stop codons
  aes(x = WCstatus, y = codon_count) + 
  geom_boxplot() +
  geom_point(size=2, alpha = 0.2, position = position_jitter(width = 0.2), aes(colour = factor(WCstatus))) +
  facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("All data codon count by aminoacid") +
  theme(plot.title = element_text(hjust = 0.5))

#Gerando plots só com sumarizações (soma, média) dos dois grupos (WC e noWC)

two_groups = dados %>%
  group_by(Aminoacid, WCstatus) %>%
  summarise(codon_sum = sum(codon_count), codon_mean = mean(codon_count), fraction_mean = mean(Fraction), .groups = "drop")

#Codom_sum
codon_sum = two_groups %>% 
         filter(Aminoacid != "End") %>%
         group_by(WCstatus) %>% 
         summarise(codon_sum = sum(codon_sum))
  
ggplot(codon_sum) +
  aes(x = WCstatus, y = codon_sum) + 
  geom_col(aes(fill = WCstatus)) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Codon count (sum) -  WC vs noWC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(two_groups) +
  aes(x = WCstatus, y = codon_sum) + 
  geom_col(aes(fill = WCstatus)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("Codon count (sum) -  WC vs noWC by aminoacid") +
  theme(plot.title = element_text(hjust = 0.5))


#Codon_mean

codon_mean = dados %>% 
         filter(Aminoacid != "End") %>%
         group_by(WCstatus) %>% 
         summarise(codon_mean = mean(codon_count))

ggplot(codon_mean) +
  aes(x = WCstatus, y = codon_mean) + 
  geom_col(aes(fill = WCstatus)) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Codon count (mean) -  WC vs noWC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(two_groups) +
  aes(x = WCstatus, y = codon_mean) + 
  geom_col(aes(fill = WCstatus)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("Codon count (mean) -  WC vs noWC by aminoacid") +
  theme(plot.title = element_text(hjust = 0.5))


#fractions
#Sum all fractions in a given species
fractions = dados %>% 
         filter(Aminoacid != "End") %>%
         group_by(Species, Aminoacid, WCstatus) %>% 
         summarise(fraction_sum = sum(Fraction))

#And then get the average fraction for all species
fractions_final = fractions %>%
         group_by(Aminoacid, WCstatus) %>%
         summarise(fraction_mean = mean(fraction_sum))

ggplot(fractions_final) +
  aes(x = WCstatus, y = fraction_mean) + 
  geom_col(aes(fill = WCstatus)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("Fraction (mean) -  WC vs noWC") +
  theme(plot.title = element_text(hjust = 0.5))


## Comparativo última posição do códon

last_pos = dados %>%
  filter(Aminoacid != "End") %>% #Removing stop codons
  mutate(last_pos = substr(Codon,3,3)) %>% # Get only 3rd nucleotide
  group_by(last_pos) %>%
  summarise(codon_sum = sum(codon_count), codon_mean = mean(codon_count))

#Plotando a ultima posição

ggplot(last_pos) +
  aes(x = last_pos, y = codon_sum) + 
  geom_col(aes(fill = last_pos)) +
  scale_y_continuous(labels = scales::comma) +
  #facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("Codon count (sum) - by 3rd codon position") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(last_pos) +
  aes(x = last_pos, y = codon_mean) + 
  geom_col(aes(fill = last_pos)) +
  scale_y_continuous(labels = scales::comma) +
  #facet_wrap(~Aminoacid, scales = "free") +
  ggtitle("Codon count (mean) - by 3rd codon position") +
  theme(plot.title = element_text(hjust = 0.5))


