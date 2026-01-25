############ STATISTICA DESCRITTIVA
############ Analisi dei parametri statistici stratificati per gruppo e variabile
############ versione 2.1 Giugno 2021

### Per scaricare il pacchetto summarytools:
# install.packages("remotes") # Using devtools is also possible
# library(remotes)
# install_github("rapporter/pander") # Necessary for optimal results!
# install_github("dcomtois/summarytools")


library(summarytools)
library(dplyr)

options(scipen=999) # esprime i numeri in formato fisso

#### Carico il dataset standard
dati_completi <- DATASET_DEMETRA_PLASMA_CONIUGATI_SOLO_GLI_ANALIZZATI # dataset classico, con ogni variabile per colonna

#Se serve rinominare le  columns
# dati_completi <- dati_completi %>% rename("Group" = "xxxx") # Ribattezzo il vettore con i gruppi "xxx", chiamandolo "Group"

#### Se ci sono altri raggruppamenti; ne seleziono uno alla volta
dati_completi <- subset(dati_completi, Timepoint == "Timepoint 2")  #Qui Timepoint indica l'altro tipo di raggruppamenti

### Seleziono solo le variabili numeriche e quella "categoriale", in qs caso Group
dati_completi <- dati_completi[, c(3, 5:9)]

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna ID
stby(data = dati_completi,             ### il dataset
     INDICES = dati_completi$Group, ### qui "Group" specifica il nome del vettore categoriale
     FUN = descr,                      ###
     stats = c("mean", "sd", "min", "med", "max", "CV"), ### specifico i parametri da misurare
     round.digits=4)                   ### round.digits=8 approssima i dati a 8 cifre decimai



#### IN ALTERNATIVA:

#Mi fa la statistica generale sui "fattori/gruppi" presenti nella colonna Group (PF e GF) 
stby(dati_completi, ### il dataset
     dati_completi$Group,  ### specifica il nome del vettore categoriale
     dfSummary,   ### comando per vedere tutti i parametri statistici
     round.digits=8)#round.digits=8 approssima i dati a 8 cifre decimali
