##### BOXMULTIPLI con analisi statistica in ggplot2
###Analisi Boari
##7 dicembre 2020

library(ggpubr)
library(reshape2)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

dati_completi = IBD_cani # dataset classico, con ogni variabile per colonna
dati_completi <- dati_completi[, -1] #elimino la prima colonna

head(dati_completi) 

#rename columns
dati_completi <- dati_completi %>% rename(Group = Phenotype, "2-AG" = "2AG")
head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati <- dati_reshaped[which(dati_reshaped$variable == "AEA" |
                              dati_reshaped$variable == "2-AG"), ]

#Riordino delle variabili categoriche
#If the levels are not automatically in the correct order, re-order them as follow:
#dati$variable <- ordered(dati$variable,
#                              levels = c("C12.0", "C14.0", "C16.0"))

###########

dati = dati_reshaped

size = 12 #qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

ggboxplot(
  dati,             # dataset
  x = "Group",       # Variabile indipendente 
  y = "value",           # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("Healthy", "PLE")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 12, vjust = 0.5) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Relative expression") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )
