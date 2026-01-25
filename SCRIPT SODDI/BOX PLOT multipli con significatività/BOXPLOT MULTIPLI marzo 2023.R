###### BOXPLOT MULTIPLI con reshaping (due o più gruppi)
### Versione 2.0: Confronto tra due o più gruppi gruppi
### 28/03/2023


#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape2)


options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 12         #  Dimensione del font del testo nel grafico

# Caricare un dataset classico, con ogni variabile per colonna
dati <- IBD_cani
head(dati)


############################# OPERAZIONI PRELIMINARI
#### Se serve selezionare certe colonne (vettori)
dati = dati[, -c(1)]

# Se serve rinominare le colone
dati <- dati %>% rename("Group" = "Phenotype") ### NB: il primo termine dell'eq. sosituisce il secondo

##### Group as factor
dati$Group <- as.factor(dati$Group)

head(dati)

#### Se serve Selezionare un fattore di un certo gruppo
#dati_8 <- subset(dati, Age == "8")

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
#dati$Group <- recode_factor(dati$Group, "nome da sostituire 1" = "nome corretto 1"
#                                      , "nome da sostituire 2" = "nome corretto 2" )

### Se serve modificare la tipologia del vettore categorico (Group)
#dati_completi$Group <- as.factor(dati_completi$Group)

#### Se serve rimuovere un fattore di un certo gruppo
#dati <- subset(dati, Genotype != "Wt")

#### Se serve Selezionare un fattore di un certo gruppo
#dati <- subset(dati, Genotype == "Tg")

### RESHAPING dei dati ################################
dati_reshaped = melt(dati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

##### Seleziono solo la variabile PEA
#dati <- dati_reshaped[which(dati_reshaped$variable == "PEA"), ]

#COMANDO per selezionare altre variabili di interesse:
#dati <- dati_reshaped[which(dati_reshaped$var== "BLC" |
#                              dati_reshaped$var == "MCP1" |
#                              dati_reshaped$var == "Eotaxin2" |
#                              dati_reshaped$var == "Fractalkine"), ]

### Prendo tutte le variabili
dati <- dati_reshaped

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Group <- ordered(dati$Group, levels = c("Healthy", "FRE", "ARE", "IBD",  "PLE"))

# Occorre cambiare il nome alla colonna variable perché fa conflitto con t_test
dati<- dati %>% rename("var" = "variable")
head(dati)

# If the levels della nuova colonna var are not automatically in the correct order, re-order them as follow:
#dati$var <- ordered(dati$var,
#                            levels = c("Eotaxin2", "MCP1", "Fractalkine", "BLC"))

#################################### Fine operazioni preliminari



##### QUI si decidono quali coppie confrontare:
#my_comparisons <- list(c("CTRL", "PEA"))
my_comparisons <- list(c("Healthy", "FRE"), c("Healthy", "ARE"), c("Healthy", "IBD"), c("Healthy", "PLE"))

stat.test <- dati %>%
  group_by(var) %>%
  t_test(value ~ Group,
         comparisons =  my_comparisons,
         #data = dati,
         #ref.group = "Healthy",
         paired = FALSE,
         var.equal = TRUE,
         alternative = "two.sided",
         #       mu = 0,
         conf.level = 0.95,
         detailed = FALSE
  ) %>%
  adjust_pvalue(method = "bonferroni") %>% ##### Qui si decidono i confronti
  add_significance()


 
stat.test 

p <- ggboxplot(
  dati,                             # dataset
  x = "Group",              # Variabile categorica 1 sulle ascisse
  y = "value",              # Nome della variabile dipendente
  color = "black",
  facet.by = "var",          # Se più di una Variabile dipendente
  scales ="free_y",
  fill ="Group",            # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg",          # Colorazione secondo Nature
  repel = TRUE,
  outlier.shape = NA,       # Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro
  add="dotplot",
  add.params = list(dotsize = 1,      #### Dimensione dei punti
                    alpha = 1, 
                    fill="white"))+  #### Riempio di bianco i punti
  
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Group), color="black", fill="red", position=position_dodge(0))+
  
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Concentration (pg/mL)") +      #nome delle ordinate
  
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 1, hjust = 0.5, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")

stat.test <- stat.test %>% add_xy_position(x = "Group", scales = c("free_y"))
p + stat_pvalue_manual(stat.test,
                       label.size = 3,    ### dimensione asterisco e numeri
                       bracket.size = 0.3, ### spessore linea
                       #bracket.nudge.y = 0.5,  ### distanza tra l'ultimo valore e la parentesi...
                       bracket.shorten = 0,  ####???
                       color = "red",
                       linetype = 1,       ####
                       step.increase = 0, ####
                       tip.length = 0.03, #### DIMENSIONE DELLA stanghetta della PARENTESI DI SIGNIFICATIVITA
                       hide.ns = TRUE,   ####### Nasconde i confronti ns
                       vjust = -0.1,   #### posizione relativa tra asterisco e linea
                       label = "{p.adj}{p.adj.signif}"
) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))



