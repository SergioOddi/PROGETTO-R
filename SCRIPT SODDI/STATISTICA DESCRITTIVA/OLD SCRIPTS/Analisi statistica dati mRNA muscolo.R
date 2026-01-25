############ STATISTICA DESCRITTIVA
############ Analisi dei parametri statistici di mRNA nel muscolo (Finalizzata Napoli)
############ 


library(summarytools)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

#### Carico il dataset standard
dati_completi <- mRNA_Napoli_xlsx_Muscolare # dataset classico, con ogni variabile per colonna

dati_completi <- dati_completi[, 3:12] #Prendo solo la colonna Dieta e le colonne con le variabili

#rename columns
dati_completi <- dati_completi %>% rename("Group" = "Dieta") # Ribattezzo il vettore con i gruppi, chiamandolo "Group"

dati_completi$Group <- as.factor(dati_completi$Group) ### Trasformo il vettore Group in un "vettore categoriale" (factor)
head(dati_completi)

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna Group (PF e GF) 
stby(dati_completi, ### il dataset
     dati_completi$Group,  ### specifica il nome del vettore categoriale
     dfSummary,   ### comando per vedere tutti i parametri statistici
     round.digits=8)#round.digits=8 approssima i dati a 8 cifre decimali
