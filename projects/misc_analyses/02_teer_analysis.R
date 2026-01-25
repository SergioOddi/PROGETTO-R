###### BOXPLOT MULTIPLI TEER
### 25/10/2023


#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape2)


options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 16         #  Dimensione del font del testo nel grafico

# Caricare un dataset classico, con ogni variabile per colonna
dati <- Dataset_ZnOAT_TEER
head(dati)


#dati[dati$TEER == 95.63, ]$TEER <- 41.25 ### in un dataset “data” nel vettore “sex” sostituisci a 0 il valore F


############################# OPERAZIONI PRELIMINARI
#### Se serve selezionare certe colonne (vettori)
#dati = dati[, -c(1)]

# Se serve rinominare le colone
#dati <- dati %>% rename("Group" = "Treatment") ### NB: il primo termine dell'eq. sosituisce il secondo

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
dati <- dati_reshaped

##### Seleziono solo la variabile PEA
#dati <- dati_reshaped[which(dati_reshaped$variable == "PEA"), ]

#COMANDO per selezionare altre variabili di interesse:
#dati <- dati_reshaped[which(dati_reshaped$var== "BLC" |
#                              dati_reshaped$var == "MCP1" |
#                              dati_reshaped$var == "Eotaxin2" |
#                              dati_reshaped$var == "Fractalkine"), ]

### Prendo tutte le variabili
#dati <- dati_reshaped

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Group <- ordered(dati$Group, levels = c("CTRL", "Inflamed", "ZnO", "HTs", "ZnO/HTs"))

# NB: Occorre cambiare il nome alla colonna "variable" perché fa conflitto con t_test
dati<- dati %>% rename("var" = "variable")
head(dati)

# If the levels della nuova colonna var are not automatically in the correct order, re-order them as follow:
#dati$var <- ordered(dati$var,
#                            levels = c("Eotaxin2", "MCP1", "Fractalkine", "BLC"))

#################################### Fine operazioni preliminari



##### QUI si decidono quali coppie confrontare:
#my_comparisons <- list(c("CTRL", "PEA"))
my_comparisons <- list(c("CTRL", "Inflamed"), c("Inflamed", "ZnO"), c("Inflamed", "HTs"), c("Inflamed", "ZnO/HTs"), c("ZnO", "ZnO/HTs"), c("ZnO", "HTs"))

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
  adjust_pvalue(method = "holm") %>% ##### Qui si decidono i confronti: bonferroni, holm, etc.
  add_significance()

stat.test 

stat.test <- stat.test %>%
  mutate(p.signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  ))

stat.test


### Palette colori
"#E64B35B2" 
"#4DBBD5B2"
"#00A087B2" 
"#3C5488B2"
"#F39B7FB2" 
"#8491B4B2"
"#91D1C2B2" 
"#DC0000B2" 
"#7E6148B2"

# Personalizzazione colori 1
colori_customizzati <- c("CTRL" = "white", "Inflamed" = "#FC4E07", "ZnO" = "#00AFBB", "HTs" = "#E7B800", "ZnO/HTs" = "steelblue")

# Personalizzazione colori secondo gruppo Nature (npg)
colori_customizzati <- c("CTRL" = "#E64B35B2", "Inflamed" = "#4DBBD5B2", "ZnO" = "#00A087B2" , "HTs" = "#3C5488B2", "ZnO/HTs" = "#F39B7FB2")


p <- ggboxplot(
  dati,                             # dataset
  x = "Group",              # Variabile categorica 1 sulle ascisse
  y = "value",              # Nome della variabile dipendente
  color = "black",
  facet.by = "var",          # Se più di una Variabile dipendente
  scales ="free_y",
  fill ="Group",            # colora i boxplot in base alla categoria presente nella colonna "Group"
# palette = "npg",          # Colorazione secondo Nature
  repel = TRUE,
  outlier.shape = NA,       # Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro
  add="dotplot",
  add.params = list(dotsize = 0.8,      #### Dimensione dei punti
                    alpha = 1, 
                    fill="white"))+  #### Riempio di bianco i punti
  scale_fill_manual(values = colori_customizzati) +
 # geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = 0.5)+ #### Aggiunge un asse orizzontale a un valore fissato
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Group), color="black", fill="red", position=position_dodge(0))+
  
  labs(title="",                           #se serve il titolo
       x = "Treatment",                        #nome delle ascisse
       y = "% TEER") +      #nome delle ordinate
  
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Helvetica"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 1, hjust = 0.5, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")

stat.test <- stat.test %>% add_xy_position(x = "Group", scales = c("free_y"))

p + stat_pvalue_manual(stat.test,
                       label.size = 3,    ### dimensione asterisco e numeri
                       bracket.size = 0.3, ### spessore linea
                       bracket.nudge.y = -15,  ### distanza tra l'ultimo valore e la parentesi...
                       bracket.shorten = 0,  ####???
                       color = "red",
                       linetype = 1,       ####
                       step.increase = 0, ####
                       tip.length = 0.03, #### DIMENSIONE DELLA stanghetta della PARENTESI DI SIGNIFICATIVITA
                       hide.ns = TRUE,   ####### Nasconde i confronti ns
                       vjust = -0.1,   #### posizione relativa tra asterisco e linea
                  #    label = "{p.adj}{p.adj.signif}"
                       label = "{p}{p.signif}"
) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))



