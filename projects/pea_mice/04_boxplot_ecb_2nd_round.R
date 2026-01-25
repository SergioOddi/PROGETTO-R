##### BOXMULTIPLI con analisi statistica in ggplot2
### Analisi livelli eCB Topi Finalizzata PEA
## 19 maggio 2021

library(ggpubr)
library(reshape2)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

dati_completi = dati_completi[-27, ]



#### ANALISI su tutti gli animali indipendentemente dal sesso e dal genotipo
#### Rimuovo le osservazioni della prima tornata

dati_completi <- subset(dati_completi, Tornata != "Prima")

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

###########



size = 12 #qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

ggboxplot(
  dati,             # dataset
  x = "Group",       # Variabile indipendente 
  y = "value",           # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot" ### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("Placebo", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 5, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )

########################
#######################

#### RIMOZIONE OUTLIER
#### ANALISI su tutti gli animali indipendentemente dal sesso e dal genotipo
#### Rimuovo le osservazioni della prima tornata
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

#rimuovo oulier
dati_completi = dati_completi[-27, ]
dati_completi = dati_completi[-20, ]
dati_completi = dati_completi[-18, ]

#### ANALISI su tutti gli animali indipendentemente dal sesso e dal genotipo
#### Rimuovo le osservazioni della prima tornata
dati_completi <- subset(dati_completi, Tornata != "Prima")
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )








#### Analisi Tg
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )

##### Analisi su "Tg maschi"
##### Rimuovo wt e le femmine
####

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo prima Tornata i Wt
dati_completi <- subset(dati_completi, Tornata != "Prima" & Sex !="F" & Genotype != "Wt")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )

#### ANALISI su "Tg femmine"
#### RIMUOVO i Wt e i maschi

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo la prima Tornata, i Wt, e i maschi
dati_completi <- subset(dati_completi, Tornata != "Prima" & Sex != "M" & Genotype != "Wt")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )

#### Analisi solo wt
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )


##### Analisi Wt maschi
##### Rimuovo Tg e le femmine

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo prima Tornata, femmine e Tg 
dati_completi <- subset(dati_completi, Tornata != "Prima" & Sex !="F" & Genotype != "Tg")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )


#### Analisi Wt femmine
##### Rimuovo Tg e i maschi

#### Carico il dataset originale e completo
dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

## Rimuovo l'outlier
dati_completi = dati_completi[-27, ]

#### Rimuovo la prima tornata, i Wt, e i maschi
dati_completi <- subset(dati_completi, Tornata != "Prima" & Sex != "M" & Genotype != "Tg")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
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

dati$Group <- ordered(dati$Group,
                         levels = c("Placebo", "PEA"))

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
    label = "p.signif", size = 4, vjust = 0.25) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )


#### ANALISI su tutti gli animali indipendentemente dal sesso e dal genotipo
#### Rimuovo le osservazioni della prima tornata

dati_completi <- subset(dati_completi, Tornata != "Prima")

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

###########



size = 12 #qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

ggboxplot(
  dati,             # dataset
  x = "Group",       # Variabile indipendente 
  y = "value",           # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  #, palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot" ### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("Placebo", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 5, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=size, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=size, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=size, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=size, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=size, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )






