#### Boxplot dei dati Topi Wt e Tg livelli PEA progetto finalizzata
### Analisi 17 novembre 2021

##### BOXMULTIPLI con analisi statistica in ggplot2

library(dplyr)
library(ggpubr)
library(reshape2)

options(scipen=999) # esprime i numeri in formato fisso

dati_completi = Campioni_sangue_Topi_FINALIZZATA_2nd_tornata # dataset classico, con ogni variabile per colonna

### Per eliminare colonne specifiche
dati_completi <- dati_completi[, -1] #elimino la prima colonna

### Per eliminare osservazioni specifiche
dati_completi <- subset(dati_completi, Tornata != "Prima") ### Comando per eliminare osservazioni caratterizzate dall'avere come fattore "Prima" nel vettore "Tornata"
dati_completi <- subset(dati_completi, Group != "PEA")
head(dati_completi)

dati_completi <- dati_completi[, -c(1, 2, 4)] #elimino la prima e la quarta colonna


#rename columns
dati_completi <- dati_completi %>% rename(Group  = Genotype)
dati_completi <- dati_completi %>% rename("PEA" = "PEA (nM)", "AEA" = "AEA (nM)", "OEA" = "OEA (nM)", "2-AG" = "2AG (nM)")
head(dati_completi)


### Riordino i gruppi
dati_completi$Group <- ordered(dati_completi$Group,
                                  levels = c("Wt", "Tg"))


### Reshaping dei dati ####################################################################################################

dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped


##### Seleziono solo la variabile PEA
dati <- dati_reshaped[which(dati_reshaped$variable == "PEA"), ]

dati


#COMANDO per selezionare altre variabili di interesse:
#dati <- dati_reshaped[which(dati_reshaped$variable == "AEA" |
#                              dati_reshaped$variable == "2-AG" |
#                              dati_reshaped$variable == "PEA"), ]

### Prendo tutte le variabili
dati <- dati_reshaped


# If the levels are not automatically in the correct order, re-order them as follow:
#dati$variable <- ordered(dati$variable,
#                              levels = c("AEA", "2-AG", "PEA"))

###########

row_names_to_remove<-c("26", "19", "22")
dati <- dati[!(row.names(dati) %in% row_names_to_remove),]
dati


s = 12 # qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

ggboxplot(
  dati,             # dataset
  x = "Group",       # Variabile indipendente 
  y = "value",           # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente
  scales ="free", col ="black", 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna Group
  , palette = "npg"     # Colorazione dei plot secondo Nature
) +
  geom_jitter(width=0.0, size = 2, alpha = 0.3) + #caratteristiche dello scatter dei dati; alpha varia la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("Wt", "Tg")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", method = "t.test", size = 3, vjust = 0.1) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Concentration (nM)") + #nome delle ordinate
  theme(text = element_text(size=s, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=s, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=s, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=s, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=s, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )



stat_compare_means

