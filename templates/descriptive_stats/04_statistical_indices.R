######## DESCRIZIONE DEI DATI ANALISI Degli INDICI STATISTICI

library(summarytools)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

#### Carico il dataset standard
dati_completi <- DATASET_DEMETRA_PLASMA_CONIUGATI_SOLO_GLI_ANALIZZATI # dataset classico, con ogni variabile per colonna

#Se serve rinominare le  columns
# dati_completi <- dati_completi %>% rename("Group" = "xxxx") # Ribattezzo il vettore con i gruppi "xxx", chiamandolo "Group"

#### Se ci sono altri raggruppamenti; ne seleziono uno alla volta
dati_completi_T0 <- subset(dati_completi, Timepoint == "Timepoint 0")  #Qui Timepoint indica l'altro tipo di raggruppamenti

### Seleziono solo le variabili numeriche e quella "categoriale", in qs caso Group
dati_completi_T0 <- dati_completi_T0[, c(3, 5:9)]

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna Group
stby(data = dati_completi_T0,             ### il dataset
     INDICES = dati_completi_T0$Group, ### qui "Group" specifica il nome del vettore categoriale
     FUN = descr,                      ###
     stats = c("mean", "sd", "min", "med", "max", "CV"), ### specifico i parametri da misurare
     round.digits=4)                   ### round.digits=8 approssima i dati a 1 cifra decimale

#### INDICI su timepoint 1
#### Se ci sono altri raggruppamenti; ne seleziono uno alla volta
dati_completi_T1 <- subset(dati_completi, Timepoint == "Timepoint 1")  #Qui Timepoint indica l'altro tipo di raggruppamenti

### Seleziono solo le variabili numeriche e quella "categoriale", in qs caso Group
dati_completi_T1 <- dati_completi_T1[, c(3, 5:9)]

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna Group
stby(data = dati_completi_T1,             ### il dataset
     INDICES = dati_completi_T1$Group, ### qui "Group" specifica il nome del vettore categoriale
     FUN = descr,                      ###
     stats = c("mean", "sd", "min", "med", "max", "CV"), ### specifico i parametri da misurare
     round.digits=4)                   ### round.digits=8 approssima i dati a 1 cifra decimale


#### INDICI su timepoint 2
#### Se ci sono altri raggruppamenti; ne seleziono uno alla volta
dati_completi_T2 <- subset(dati_completi, Timepoint == "Timepoint 2")  #Qui Timepoint indica l'altro tipo di raggruppamenti

### Seleziono solo le variabili numeriche e quella "categoriale", in qs caso Group
dati_completi_T2 <- dati_completi_T2[, c(3, 5:9)]

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna Group
stby(data = dati_completi_T2,             ### il dataset
     INDICES = dati_completi_T2$Group, ### qui "Group" specifica il nome del vettore categoriale
     FUN = descr,                      ###
     stats = c("mean", "sd", "min", "med", "max", "CV"), ### specifico i parametri da misurare
     round.digits=4)                   ### round.digits=8 approssima i dati a 1 cifra decimale



