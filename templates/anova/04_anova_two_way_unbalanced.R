###### ANOVA a due vie per disegni non bilanciati (unbalanced design)
#### Versione 1.2
#### Febbraio 2023

#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)
library(car)
library(rstatix)

#### Carico i dati
dati <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset

############################# OPERAZIONI PRELIMINARI

### Se occorre cambiare il "nome" a un vettore (i.e., al nome del vettore colonna)
#dati <- dati %>% rename("nome corretto" = "nome da sostituire") 

######## Se occorre rinominare il nome dei livelli all'interno di un vettore categoriale (es., GROUP)
#dati$Group <- recode_factor(dati$GROUP, "nome da sostituire 1" = "nome corretto 1"
#                                      , "nome da sostituire 2" = "nome corretto 2")

#### Se serve modificare la tipologia del vettore categorico (Group)
dati$Group <- as.factor(dati$Group)
dati$Genotype <- as.factor(dati$Genotype)
dati$Treatment <- as.factor(dati$Treatment)
dati$Age <- as.factor(dati$Age)
head(dati)

#### Se serve selezionare certe colonne (vettori)
#dati = dati[, c(3:10)]
#head(dati)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati$Genotype <- ordered(dati$Genotype, levels = c("Wt", "Tg"))

#### Se serve rimuovere oulier
#dati = dati[ , ]

#### Se serve rimuovere un fattore di un certo gruppo
#dati_8 <- subset(dati, Age != "10")

#### Se serve Selezionare un fattore di un certo gruppo
dati_8 <- subset(dati, Age == "8")
dati_12 <- subset(dati, Age == "12")

#######
Anova(lm(FC~Treatment*Genotype, data = dati_8, contrasts=list(Treatment = contr.sum, Genotype = contr.sum)), type = "III") ### istruzione equivalente a GraphPad per disegni sbilanciati

Anova(lm(FC~Treatment*Genotype, data = dati_12, contrasts=list(Treatment = contr.sum, Genotype = contr.sum)), type = "III")


res.aov <- dati_8 %>% anova_test(NO ~ Genotype * Treatment)
res.aov


cv <- Anova(lm(FC~Treatment*Genotype, data = dati_8, contrasts=list(Treatment = contr.sum, Genotype = contr.sum)), type = "III") ### istruzione equivalente a GraphPad per disegni sbilanciati


##### Post hoc test
TukeyHSD(cv, which = 'WT:Tg')

