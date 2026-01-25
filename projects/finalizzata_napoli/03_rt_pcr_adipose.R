##### BOXMULTIPLI con analisi statistica in ggplot2
### Analisi livelli sistema eCB Topi Finalizzata NAPOLI 
#### TESSUTO ADIPOSO
## 14 maggio 2021

library(ggpubr)
library(reshape2)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

dati_completi = Dati_espressione_genica_finalizzata_Napoli_xlsx_Adiposo # dataset classico, con ogni variabile per colonna
dati_completi <- dati_completi[, c(2,3,4,5,11)] #Prendo solo la colonna gruppi e le colonne con le variabili

head(dati_completi) 

#rename columns
#dati_completi <- dati_completi %>% rename(Group = Dieta)
#head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Dieta") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

dati <- dati_reshaped


#COMANDO per selezionare le variabili di interesse:
#dati <- dati_reshaped[which(dati_reshaped$variable == "PEA" |
#                              dati_reshaped$variable == "AEA" |
#                              dati_reshaped$variable == "OEA" |
#                              dati_reshaped$variable == "2AG"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
#dati$variable <- ordered(dati$variable,
#                         levels = c("PEA", "OEA", "AEA", "2AG"))

###########



size = 12 #qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

ggboxplot(
  dati,             # dataset
  x = "Dieta",       # Variabile indipendente 
  y = "value",           # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Dieta" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("1", "2"), c("2", "3"), c("1", "3") ), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
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

########################
#######################



#### RIMUOVO i Wt

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo prima Tornata i Wt
dati_completi <- subset(dati_completi, Tornata != "Prima" & Genotype != "Wt")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
dati_completi


dati_completi <- dati_completi[, 5:9] #Prendo solo la colonna gruppi e le colonne con le variabili

head(dati_completi) 

#rename columns
dati_completi <- dati_completi %>% rename("AEA" = "AEA (nM)", "2AG" = "2AG (nM)", "PEA" = "PEA (nM)", "OEA" = "OEA (nM)")
head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati <- dati_reshaped[which(dati_reshaped$variable == "PEA" |
                              dati_reshaped$variable == "AEA" |
                              dati_reshaped$variable == "OEA" |
                              dati_reshaped$variable == "2AG"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
dati$variable <- ordered(dati$variable,
                         levels = c("PEA", "OEA", "AEA", "2AG"))


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
    comparisons = list(c("Placebo", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
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


#### RIMUOVO i Tg

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo prima Tornata e Tg
dati_completi <- subset(dati_completi, Tornata != "Prima" & Genotype != "Tg")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
dati_completi


dati_completi <- dati_completi[, 5:9] #Prendo solo la colonna gruppi e le colonne con le variabili

head(dati_completi) 

#rename columns
dati_completi <- dati_completi %>% rename("AEA" = "AEA (nM)", "2AG" = "2AG (nM)", "PEA" = "PEA (nM)", "OEA" = "OEA (nM)")
head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati <- dati_reshaped[which(dati_reshaped$variable == "PEA" |
                              dati_reshaped$variable == "AEA" |
                              dati_reshaped$variable == "OEA" |
                              dati_reshaped$variable == "2AG"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
dati$variable <- ordered(dati$variable,
                         levels = c("PEA", "OEA", "AEA", "2AG"))


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
    comparisons = list(c("Placebo", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
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


