######## DESCRIZIONE DELLA STATISTICA DI UNA VARIABILE
######## Versione 1.0 05/02/2022


library(dplyr)
library(Hmisc)

dati <- Attività_SCIENTIFICA_Produttività_Pubblicazioni
head(dati)

describe(dati)

### Filtrare in base all'anno i dati
dati_dopo2009 <- dati %>%
  filter(ANNO > 2009)

dati_dopo2009$IF_2022 <- as.numeric(dati_dopo2009$IF_2022)
dati_dopo2009$`Citazioni 01_22` <- as.numeric(dati_dopo2009$`Citazioni 01_22`)

describe(dati_dopo2009)

