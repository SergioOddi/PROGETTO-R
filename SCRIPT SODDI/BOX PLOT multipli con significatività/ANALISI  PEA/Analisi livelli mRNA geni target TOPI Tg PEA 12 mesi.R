##### Analisi statistica mRNAs topi Tg/PEA con BOXPLOT MULTIPLI in ggplot2
##### 
##### Campioni: HIPP di topi Tg2576 di 12 mesi Finalizzata PEA
##### 24 maggio 2021


library(ggpubr)
library(reshape2)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso
size = 12 #qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

dati_completi <- Geni_HIPP_Topi_Tg_PEA_12_mesi_Exp_n_  # dataset classico, con ogni variabile per colonna

#### Nel caso occorra rimuovere uno o più outlier
#dati_completi = dati_completi[-27, ]


########################## ANALIZZO tutti gli animali indipendentemente dal sesso

dati <- dati_completi[, 4:9] #Prendo solo la colonna "Group" e le colonne con le variabili

head(dati) 

#Nel caso occorra rinominare le colonne
#dati_completi <- dati_completi %>% rename("AEA" = "AEA (nM)", "2AG" = "2AG (nM)", "PEA" = "PEA (nM)", "OEA" = "OEA (nM)")
#head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati_reshaped <- dati_reshaped[which(dati_reshaped$variable == "iNOS" |
                                       dati_reshaped$variable == "IL1B" |
                                       dati_reshaped$variable == "IL6" |
                                       dati_reshaped$variable == "MCP1"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
dati_reshaped$variable <- ordered(dati_reshaped$variable,
                                  levels = c("IL6", "IL1B", "iNOS", "MCP1"))

###########
ggboxplot(
  dati_reshaped,             # dataset
  x = "Group",               # Variabile indipendente 
  y = "value",               # Variabile dipendente
  facet.by = "variable",     # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("PLACEBO", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Fold increase over HK") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )



############################ SELEZIONO solo i maschi
#### Nel caso occorra rimuovere alcune tipologie di le osservazioni
dati_selezionati <- subset(dati_completi,  Sex == "M")

dati <- dati_selezionati[, 4:9] #Prendo solo la colonna Group e le colonne con le variabili

head(dati) 

#rename columns
#dati_completi <- dati_completi %>% rename("AEA" = "AEA (nM)", "2AG" = "2AG (nM)", "PEA" = "PEA (nM)", "OEA" = "OEA (nM)")
#head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped



#COMANDO per selezionare le variabili di interesse:
dati_reshaped <- dati_reshaped[which(dati_reshaped$variable == "iNOS" |
                              dati_reshaped$variable == "IL1B" |
                              dati_reshaped$variable == "IL6" |
                              dati_reshaped$variable == "MCP1"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
dati_reshaped$variable <- ordered(dati_reshaped$variable,
                         levels = c("IL6", "IL1B", "iNOS", "MCP1"))

ggboxplot(
  dati_reshaped,             # dataset
  x = "Group",               # Variabile indipendente 
  y = "value",               # Variabile dipendente
  facet.by = "variable",     # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("PLACEBO", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Fold increase over HK") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )

########################
#######################

#### SELEZIONO le femmine

#### Nel caso occorra rimuovere alcune tipologie di le osservazioni
dati_selezionati <- subset(dati_completi,  Sex == "F")

dati <- dati_selezionati[, 4:9] #Prendo solo la colonna Group e le colonne con le variabili


#rename columns
#dati_completi <- dati_completi %>% rename("AEA" = "AEA (nM)", "2AG" = "2AG (nM)", "PEA" = "PEA (nM)", "OEA" = "OEA (nM)")
#head(dati_completi)

#reshaping del dataset
dati_reshaped = melt(dati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped


#COMANDO per selezionare le variabili di interesse:
dati_reshaped <- dati_reshaped[which(dati_reshaped$variable == "iNOS" |
                                       dati_reshaped$variable == "IL6" |
                                       dati_reshaped$variable == "MCP1"), ]

#Riordino delle variabili categoriche
# since the levels are not automatically in the correct order, re-order them as follow:
dati_reshaped$variable <- ordered(dati_reshaped$variable,
                                  levels = c("IL6", "iNOS", "MCP1"))
ggboxplot(
  dati_reshaped,             # dataset
  x = "Group",               # Variabile indipendente 
  y = "value",               # Variabile dipendente
  facet.by = "variable",     # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("PLACEBO", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Fold increase over HK") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )





#### RIMOZIONE OUTLIER
#### ANALISI su tutti gli animali indipendentemente dal sesso e dal genotipo
#### Rimuovo le osservazioni della prima tornata
dati_completi =  # dataset classico, con ogni variabile per colonna

#rimuovo oulier
dati_completi = dati_completi[-27, ]
dati_completi = dati_completi[-20, ]
dati_completi = dati_completi[-18, ]










