#### ANALISI Dati PCR Domenico
# Boxplot dei dati
### 3 febbraio 2021

library("reshape2")
library("dplyr")
library("plotly")
library("ggplot2")
library("ggpubr")

dati <- Dataset_Domenico_PCR_M1_M0_macrofagi_2_
head(dati)
### il dataset ha la forma seguente
# A tibble: 6 x 11
#   ID      Group   Polarization Treatment Subjects    MRC1   TGM2   CXCL8       IRF4        IDO1   `COX-2`
# <chr>    <chr>   <chr>        <chr>     <chr>      <dbl>  <dbl>   <dbl>      <dbl>       <dbl>     <dbl>
#1 ct21 m0 CTRL_M0 M0           Vehicle   Healthy  0.00364 0.0133 0.0168   0.000366   0.000021   0.000680 
#2 c22 m0  CTRL_M0 M0           Vehicle   Healthy  0.00150 0.0392 0.0266   0.0000799  0.0000176  0.00174  
#3 c20 m0  CTRL_M0 M0           Vehicle   Healthy  0.0133  0.0634 0.0132  NA          0.000780   0.000904 
#4 ct24 m0 CTRL_M0 M0           Vehicle   Healthy  0.00243 0.0156 0.00366  0.0000109  0.00000611 0.0000612
#5 ct25 m0 CTRL_M0 M0           Vehicle   Healthy  0.0115  0.0116 0.0110   0.000239   0.000224   0.000229 
#6 ct23 m0 CTRL_M0 M0           Vehicle   Healthy  0.00143 0.0112 0.00736  0.000112  NA          0.000770 

str(dati)

# SELEZIONO UN GRUPPO : M0
## SELEZIONO UN GRUPPO SPECIFICO
dati <- subset(dati, Phenotype == "M0") #seleziono solo  le osservazioni del gruppo (Phenotype) M0
dati

#Seleziono le colonne di interesse
dati <- dati[, c(2, 5:14)]

# rename columns
#dati <- dati %>% rename( "IL10" = "IL-10")

#RESHAPING DATAset
dati = melt(dati, id.var ="Group")
head(dati)

# IL reshaping trasforma il dataset usando la variabile Group come variabile per categorizzare le variabili analizzate (le varie proteine)
#Group       variable   value
#1 CTRL_M0       ID     ct21 m0
#2 CTRL_M0       ID     c22 m0
#3 CTRL_M0       ID     c20 m0
#4 CTRL_M0       ID     ct24 m0
#5 CTRL_M0       ID     ct25 m0
#6 CTRL_M0       ID     ct23 m0


# Eventualmente rimuovo uno o più livelli per avere un certo numero di gruppi
#dati <- subset(dati, variable != "ID")
#summary(dati)

# Eventualmente seleziono uno o più livelli del nuovo vettore "variable"
# dati <- dati[which(dati$variable == "CXCL8" |
                     dati$variable == "MRC1" |
                     dati$variable == "IRF4" |
                     dati$variable == "IDO1" |
                     dati$variable == "COX2" |
                     dati$variable == "TGM2"), ]

str(dati)

#il dataset dati ora contiene un vettore con tre quattro gruppi (Group), un vettore "variable" con sei geni e un vettore con i loro valori numerici (value)

dati$Group <- as.factor(dati$Group)
dati$value <- as.numeric(dati$value)

str(dati)


p <- ggboxplot(
  dati, x = "Group", y = "value", 
  facet.by = "variable",
  scales ="free", col ="black", fill ="Group", 
) +
  geom_jitter(width=0.0, alpha=0.2) + #caratteristiche dello scatter dei dati
  #  stat_compare_means(
  #  comparisons = list(c("CTRL", "URB")), 
  #  label = "p.signif", size =5, vjust = 0.3) + #label: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x="Treatment", #nome delle ascisse
       y = "Normalized value") # + #nome delle ordinate
#theme(text = element_text(size = 18, face="bold", color ="black", family = "Arial"), #qui size stabilisce la dimensione del testo delle variabili nel box
#      axis.text.x = element_text(angle = 45, hjust = 1), 
#      axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
#      axis.title=element_text(size=16, face="bold", color="black"),
#     legend.position = "bottom"
#) 

#per vederlo come ggplot2:
p#il rombo indica la media



#per vederlo in plotly:
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere





############   ANALISI M0 M1 15_LOX
dati <- Dataset_Domenico_PCR_M1_M0_macrofagi_2_

# SELEZIONO UN GRUPPO : M1
## SELEZIONO UN GRUPPO SPECIFICO
#dati <- subset(dati, Phenotype == "M1")

#Seleziono le colonne di interesse
dati <- dati[-c(22), c(2, 5:14)]

#rename columns
#dati <- dati %>% rename( "IL10" = "IL-10")

#RESHAPING DATAset
dati = melt(dati, id.var ="Group")

###riordinare fattori (o livelli)
dati$Group <- ordered(dati$Group,
                       levels = c("CTR_M0", "CTR_M1", "CP_M0", "CP_M1",
                                  "P4_M0", "P4_M1", "CP+P4_M0", "CP+P4_M1") )


# Eventualmente seleziono uno o più livelli del nuovo vettore "variable"
#dati <- dati[which(dati$variable == "LOX15" |
                     dati$variable == "TGM2" |
                     dati$variable == "IDO1"|
                     dati$variable == "STAT1"), ]


# Eventualmente seleziono uno o più livelli del nuovo vettore "variable"
#dati <- dati[which(dati$variable == "IL1b"), ]



#il dataset dati ora contiene un vettore con tre quattro gruppi (Group), un vettore "variable" con sei geni e un vettore con i loro valori numerici (value)

dati$Group <- as.factor(dati$Group)
dati$value <- as.numeric(dati$value)



p <- ggboxplot(
  dati, x = "Group", y = "value", 
  facet.by = "variable",
  scales ="free", col ="black", fill ="Group", 
) +
  geom_jitter(width=0.0, alpha=0.2) + #caratteristiche dello scatter dei dati
  #  stat_compare_means(
  #  comparisons = list(c("CTRL", "URB")), 
  #  label = "p.signif", size =5, vjust = 0.3) + #label: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x="Treatment", #nome delle ascisse
       y = "Normalized value") # + #nome delle ordinate
#theme(text = element_text(size = 18, face="bold", color ="black", family = "Arial"), #qui size stabilisce la dimensione del testo delle variabili nel box
#      axis.text.x = element_text(angle = 45, hjust = 1), 
#      axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
#      axis.title=element_text(size=16, face="bold", color="black"),
#     legend.position = "bottom"
#) 


#per vederlo come ggplot2:
p#il rombo indica la media

#per vederlo in plotly:
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere


############   ANALISI M1
dati <- Dataset_Domenico_PCR_M1_M0_macrofagi_2_

# SELEZIONO UN GRUPPO : M1
## SELEZIONO UN GRUPPO SPECIFICO
dati <- subset(dati, Phenotype == "M1")


#Seleziono le colonne di interesse
dati <- dati[, c(2, 5:14)]

#rename columns
#dati <- dati %>% rename( "IL10" = "IL-10")

#RESHAPING DATAset
dati = melt(dati, id.var ="Group")
head(dati)



#il dataset dati ora contiene un vettore con tre quattro gruppi (Group), un vettore "variable" con sei geni e un vettore con i loro valori numerici (value)

dati$Group <- as.factor(dati$Group)
dati$value <- as.numeric(dati$value)

str(dati)


p <- ggboxplot(
  dati, x = "Group", y = "value", 
  facet.by = "variable",
  scales ="free", col ="black", fill ="Group", 
) +
  geom_jitter(width=0.0, alpha=0.2) + #caratteristiche dello scatter dei dati
  #  stat_compare_means(
  #  comparisons = list(c("CTRL", "URB")), 
  #  label = "p.signif", size =5, vjust = 0.3) + #label: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x="Treatment", #nome delle ascisse
       y = "Normalized value") # + #nome delle ordinate
#theme(text = element_text(size = 18, face="bold", color ="black", family = "Arial"), #qui size stabilisce la dimensione del testo delle variabili nel box
#      axis.text.x = element_text(angle = 45, hjust = 1), 
#      axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
#      axis.title=element_text(size=16, face="bold", color="black"),
#     legend.position = "bottom"
#) 


#per vederlo come ggplot2:
p#il rombo indica la media

#per vederlo in plotly:
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere





