###### BOXPLOT MULTIPLI con reshaping
### Versione 1.0: Confronto tra due gruppi (t test)
### 19 marzo 2023


#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)
library(reshape2)

options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 12         #  Dimensione del font del testo nel grafico

# Caricare un dataset classico, con ogni variabile per colonna
dati <- Carl

head(dati)


############################# OPERAZIONI PRELIMINARI
#dati$Genotype <- as.factor(dati$Genotype)
#dati$Treatment <- as.factor(dati$Treatment)
#head(dati)

#### Se serve Selezionare un fattore di un certo gruppo
#dati_8 <- subset(dati, Age == "8")

# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("2AG" = "2-AG") ### NB: il primo termine dell'eq. sosituisce il secondo
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
#dati$GROUP <- recode_factor(dati$GROUP, "nome da sostituire 1" = "nome corretto 1"
#                                      , "nome da sostituire 2" = "nome corretto 2" )

### Se serve modificare la tipologia del vettore categorico (Group)
#dati_completi$Group <- as.factor(dati_completi$Group)

#### Se serve selezionare certe colonne (vettori)
dati = dati[, -c(1)]
#head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati$Treatment <- ordered(dati$Treatment, levels = c("CTRL", "KT172", "LPS", "LPS_KT172"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

### RESHAPING dei dati ####################################################################################################

dati_reshaped = melt(dati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

##### Seleziono solo la variabile PEA
#dati <- dati_reshaped[which(dati_reshaped$variable == "PEA"), ]

#COMANDO per selezionare altre variabili di interesse:
#dati <- dati_reshaped[which(dati_reshaped$variable == "AEA" |
#                              dati_reshaped$variable == "2-AG" |
#                              dati_reshaped$variable == "PEA"), ]

### Prendo tutte le variabili
dati <- dati_reshaped

# If the levels are not automatically in the correct order, re-order them as follow:
#dati$variable <- ordered(dati$variable,
#                              levels = c("AEA", "2-AG", "PEA"))




#################################### Fine operazioni preliminari


p <- ggboxplot(
  dati,                     # dataset
  x = "Group",              # Variabile categorica 1 sulle ascisse
  y = "value",              # Nome della variabile dipendente
  color = "black",
  order =c("GF", "PF"),  # per ordinare i fattori
  facet.by = "variable",    # Se più di una Variabile dipendente
# scales ="free",           # Rende automatica la scelta del range numerico dell'asse y
  scales ="free_y",
  fill ="Group",            # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg",          # Colorazione secondo Nature
  repel = TRUE,
  outlier.shape = 19,       # Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro
  add="dotplot",
  # add = c("mean", "mean_sd", "dotplot"), 
  add.params = list(dotsize = 1,      #### Dimensione dei punti
                    alpha = 1, 
                    fill="white"))+  #### Riempio di bianco i punti
  
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Group), color="black", fill="red", position=position_dodge(0))+
  
  stat_compare_means(aes(group=Group), method= "t.test",
                     paired = FALSE,                               ###### t test con dati non appaiati
                     method.args = list(var.equal = TRUE,          ###### Varianza uguale?      
                                        p.adjust.method = "bonf"), ###### se sono solo due gruppi: t.test
                     ### label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 25 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  
  geom_signif(comparisons = list(c("GF", "PF")), map_signif_level=TRUE,
              size = 0.3,    #### DIMENSIONE DELLA PARENTESI DI SIGNIFICATIVITA
              tip_length=0.02,  #### DIMENSIONE DELLA stanghetta della PARENTESI DI SIGNIFICATIVITA
              vjust = 0.15,    #### posizione asterischi sulla parentesi
  #           margin_top = 0.2,
  #           y_position= 
              # y_position = 0.3,
              # manual=TRUE,
              textsize = 3.5) + ###### Dimensione asterisco o NS

  labs(title="",                           #se serve il titolo
       x = "Genotype",                        #nome delle ascisse
       y = "FC") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 1, hjust = 0.5, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")

p   ### Visualizza il boxplot  











