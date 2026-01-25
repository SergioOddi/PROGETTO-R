######### MATRICE DI CORRELAZIONE stratificata per GRUPPI
######### Versione 02
######### 10 giugno 2021

##### Librerie
library("tidyverse")
library(GGally)

#### Importo il dataset classico
dati <- data_cani
#### Se occorre rinominare le colonne
dati <- dati %>% rename("AEA" = "AEA pmol/ml")
dati <- dati %>% rename("AG" = "2AG pmol/ml")
dati <- dati %>% rename("PEA" = "PEA pmol/ml")
dati <- dati %>% rename("OEA" = "OEA pmol/ml")

#### Se occorre escludere qualche gruppo
dati <- subset(dati, Group != "healthy")


#### Seleziono il vettore coi diversi fattori (Group) e le variabili su cui intendo valutare la correlazione
dati3 <- dati[, c(2,10:13,14)]
head(dati3)

#MATRICE di correlazione con i dati raggruppati per FATTORI
dati3 %>% ggpairs(.,  legend = 1,
                  
                  mapping = ggplot2::aes(colour=Group), #Group è la colonna con la variabile categorica con due fattori GF e PF
                  lower = list(continuous = wrap("smooth", alpha = 0.3, size=1.5), #qui size determina la dimensione del punto
                               discrete = "blank", combo="blank"), 
                  diag = list(discrete="barDiag", 
                              continuous = wrap("densityDiag", alpha=0.5 )), 
                  upper = list(combo = wrap("box_no_facet", alpha=0.5),
                               continuous = wrap("cor", size=3, alignPercent=0.8))) + #qui size determina la dimensione del testo
  theme(panel.grid.major = element_blank())    # remove gridlines