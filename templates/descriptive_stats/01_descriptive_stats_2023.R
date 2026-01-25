############ STATISTICA DESCRITTIVA
############ Analisi dei parametri statistici stratificati per gruppo(i) e variabile
############ Versione 2.2 Gennaio 2023


library(dplyr)
options(scipen=999) # esprime i numeri in formato fisso
options(digits=4)

dati <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset

### Se occorre cambiare il "nome" a un vettore (i.e., al nome del vettore colonna)
#dati <- dati %>% rename("nome corretto" = "nome da sostituire") 

######## Se occorre rinominare il nome dei livelli all'interno di un vettore categoriale (es., GROUP)
#dati$GROUP <- recode_factor(dati$GROUP, "nome da sostituire 1" = "nome corretto 1"
#                                      , "nome da sostituire 2" = "nome corretto 2")




#### VERSIONE snella
### ANALISI dati di "Fear conditioning"
dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: GROUP, TIMEPOINT, ecc.)
  summarise(
    count = n(),
    median = median(FC, na.rm = TRUE),   #### AEA il nome della variabile da analizzare
    mean = mean(FC, na.rm = TRUE),          #### AEA il nome della variabile da analizzare
    sd = sd(FC, na.rm = TRUE),                       #### AEA il nome della variabile da analizzare
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )

datiFC <- dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: GROUP, TIMEPOINT, ecc.)
  summarise(
    count = n(),
    median = median(FC, na.rm = TRUE),   #### AEA il nome della variabile da analizzare
    mean = mean(FC, na.rm = TRUE),          #### AEA il nome della variabile da analizzare
    sd = sd(FC, na.rm = TRUE),                       #### AEA il nome della variabile da analizzare
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )

# Export DataFrame to CSV file
write.csv(datiFC,file='/Users/sergiooddi/Library/Mobile Documents/com~apple~CloudDocs/R/ANALISI/indiciFC.csv')

###Analisi test YM
datiYM <- dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: GROUP, TIMEPOINT, ecc.)
  summarise(
    count = n(),
    median = median(YM, na.rm = TRUE),   #### AEA il nome della variabile da analizzare
    mean = mean(YM, na.rm = TRUE),          #### AEA il nome della variabile da analizzare
    sd = sd(YM, na.rm = TRUE),                       #### AEA il nome della variabile da analizzare
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )
datiYM

write.csv(datiYM,file='/Users/sergiooddi/Library/Mobile Documents/com~apple~CloudDocs/R/ANALISI/indiciYM.csv')

###Analisi test NORT
datiNORT <- dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: GROUP, TIMEPOINT, ecc.)
  summarise(
    count = n(),
    median = median(NORT, na.rm = TRUE),   #### AEA il nome della variabile da analizzare
    mean = mean(NORT, na.rm = TRUE),          #### AEA il nome della variabile da analizzare
    sd = sd(NORT, na.rm = TRUE),                       #### AEA il nome della variabile da analizzare
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )
datiNORT

write.csv(datiNORT,file='/Users/sergiooddi/Library/Mobile Documents/com~apple~CloudDocs/R/ANALISI/indiciNORT.csv')

#### Test supplementare
dati <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset
#### Seleziono il gruppo a 10 mesi di età
dati <- subset(dati, Age == "10")


###Analisi test NORT a 10 mesi
datiNORT <- dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: GROUP, TIMEPOINT, ecc.)
  summarise(
    count = n(),
    median = median(NORT, na.rm = TRUE),   #### AEA il nome della variabile da analizzare
    mean = mean(NORT, na.rm = TRUE),          #### AEA il nome della variabile da analizzare
    sd = sd(NORT, na.rm = TRUE),                       #### AEA il nome della variabile da analizzare
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )
datiNORT

write.csv(datiNORT,file='/Users/sergiooddi/Library/Mobile Documents/com~apple~CloudDocs/R/ANALISI/indiciNORT_10mesi.csv')




#### VERSIONE COMPLETA ##### min, max e IQR (range interquartile)
dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare una, due o più vettori categorici.... (factors)
  summarise(
    count = n(),
    median = median(FC, na.rm = TRUE),
    mean = mean(FC, na.rm = TRUE),
    sd = sd(FC, na.rm = TRUE),
    CV= sd/mean*100,
    min = min(FC, na.rm = TRUE),
    max = max(FC, na.rm = TRUE),
    IQR = IQR(FC, na.rm = TRUE)
  )
