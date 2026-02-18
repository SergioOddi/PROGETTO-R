################ BOXPLOT MULTIPLI con reshaping del dataset

####### 3 giugno 2021

#### LIBRERIE
library(ggpubr)
library(dplyr)
library(reshape2)


options(scipen = 999) # Esprime i numeri in formato fisso
sizeT <- 12         # Dimensione del font del testo nel grafico

# Dataset classico, con ogni variabile per colonna
dati_completi <- Dan
head(dati_completi)

#  ID     Genotype Group   Nitrotyrosine     Actin      Ratio
# <chr>   <ord>    <chr>        <dbl>       <dbl>       <dbl>
#1 WT Veh Wt       PLACEBO      80800       41000        1.97
#2 WT Veh Wt       PLACEBO      57700       47400        1.22
#3 TG Veh Tg       PLACEBO      112000      40500        2.77

############################# OPERAZIONI PRELIMINARI

# Se serve rinominare le colone
dati_completi <- dati_completi %>% rename("Group" = "GROUP") ### NB: il primo termine dell'eq. sosituisce il secondo
head(dati_completi)

#### Se serve modificare la tipologia del vettore categorico (Genotype)
dati_completi$Genotype <- as.factor(dati_completi$Genotype)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati_completi$Genotype <- ordered(dati_completi$Genotype,
                                  levels = c("Wt", "Tg"))
#### Se serve rimuovere oulier
dati_completi <- dati_completi[, -c(1, 2)]

#### Se serve rimuovere osservazioni di un certo gruppo
dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare osservazioni di un certo gruppo
dati_completi <- subset(dati_completi, Genotype == "Tg")

############### 


#### Reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #Riformatto il dataset in base ai fattori presenti nella colonna "Genotype_Group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati_reshaped <- dati_reshaped[which(dati_reshaped$variable == "Nitrotyrosine" |
                              dati_reshaped$variable == "Actin" |
                              dati_reshaped$variable == "Ratio"), ]

#### Se serve modificare l'ordine di comparsa dei Gruppi
dati_reshaped$Genotype_Group <- ordered(dati_reshaped$Genotype_Group,
                                  levels = c("WT Veh", "WT PEA", "TG Veh", "TG PEA"))

#################################### Fine operazioni preliminari ################################

#### Verificare che la colonna value sia numerica:
dati_reshaped$value <- as.numeric(dati_reshaped$value)



########################### Boxplot

dati <- dati_reshaped

p <- ggboxplot(
  dati,                   # dataset
  x = "Group",   # Variabile indipendente 
  y = "value",            # Valore numerico della variabile
  facet.by = "variable",  # Se più di una variabile
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group", # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg"       # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  stat_compare_means(
    comparisons = list(c("CTRL", "PEA")), # Qui si può decidere le coppie su cui fare la significatività statistica 
    #in base al numero di trattamenti
    label = "p.signif", size = 5, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco, 
    #size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "3-NT/actin") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )

p



#################################### Fine operazioni preliminari
p <- ggboxplot(
  dati,                   # dataset
  x = "Group",   # Variabile indipendente 
  y = "value",            # Valore numerico della variabile
  facet.by = "variable",  # Se più di una variabile
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group", # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg"       # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA,   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  add="dotplot",
  add.params = list(dotsize = 2, alpha = 1, fill="white"))+
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Group), color="black", fill="red", position=position_dodge(0))+
  stat_compare_means(comparisons = list(c("CTRL", "PEA")),
    aes(group=Group), method= "t.test", 
    method.args = list(var.equal = TRUE),
    p.adjust.method = "bonf",  
    label = "p.signif" #### mostra asterischi e ns
    # label = "signif(p,5)"
    , tip.length = 0.01
    , size = 3
    , vjust = -0.1
  )+ 
  labs(title="",                           #se serve il titolo
       x = "Treament",                        #nome delle ascisse
       y = "") +                              #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=8, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=8, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=8, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=8, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")
p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)))   ### Visualizza il boxplot  



geom_bracket(data = stat.test,     #Graffette di significatività
             aes(label = signif(p, 3)),     #mostra il p value con il numero di cifre significative ; se si vuole gli asterischi:  aes(label = p.signif), 
             tip.length = 0.04, # dimensione della punta delle graffette
             y.position = 0.48, # posizione della graffetta sulle ordinate
             label.size = 5, # dimensione del testo sulla graffetta
             type = c("text", "expression"),
             na.rm = FALSE,
             show.legend = FALSE,
             bracket.nudge.y = 0,
             bracket.shorten = 0,
             family = "",
             size = 0.3)







######  Per vederlo in plotly

library("plotly")
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere





