####### RICERCA e rimozione di OUTLIER
####### Versione 1.0 21/Febbraio/2021


data <- data_cani
data <- data[, -c(1, 4, 5, 6)]
head(data)

colnames(data) <- c(
  "group",
  "class",
  "sex",
  "age",
  "body_weight",
  "AEA",
  "AG2",
  "PEA",
  "OEA"
)

data <- data[, c(1, 2, 4:9)]

##### Boxplot multipli....
library(reshape2)
library(ggpubr)

dati_reshaped = melt(data, id.var ="group") #riformatto il dataset in base ai fattori presenti nella colonna "group"
dati_reshaped

#COMANDO per selezionare le variabili di interesse:
dati <- dati_reshaped[which(dati_reshaped$variable == "AEA" |
                              dati_reshaped$variable == "AG2" |
                              dati_reshaped$variable == "OEA"  |
                              dati_reshaped$variable == "PEA"),  ]

taglia = 12 # qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

dati$value <- as.numeric(dati$value)
dati$group <- as.factor(dati$group)
glimpse(dati)

ggboxplot(
  dati,                               # dataset
  x = "group",                 # Variabile indipendente 
  y = "value",                  # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente.... chiarire meglio...
  scales ="free", col ="black", 
  fill ="group" # colora i boxplot in base alla categoria presente nella colonna group
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha varia la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("healthy", "FRE")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 12, vjust = 0.5) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Relative expression") + #nome delle ordinate
  theme(text = element_text(size=taglia, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=taglia, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=taglia, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=taglia, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=taglia, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )


#### Rimozione degli outlier
summary(data)

grouped2 <- data %>%
group_by(group) %>%
  summarise(min_AEA =  min(AEA), avg_AEA =mean(AEA), max_AEA = max(AEA),
            med_AEA = median(AEA), IQR_AEA = IQR(AEA))

grouped2



###Potrei selezionando un gruppo alla volta rimuovere gli outlier e poi rimettere su tutto il dataset...


##### In alternativa
# Alternatively, a Outliers() function is created.
# The same function is applied to multiple variables at the same time using the filter_at() function.
Outliers <- function(x) (x <= quantile(x, .75) + (1.5 * IQR(x)))
healthy1 <- data %>%
  subset (group == "healthy") %>%
  filter_at(
    c(
      'AEA',
      'AG2',
      'OEA',
      'PEA'
    ),
    Outliers
  )

healthy1


summarise(healthy1, min_AEA =  min(AEA), avg_AEA =mean(AEA), max_AEA = max(AEA),
          med_AEA = median(AEA), IQR_AEA = IQR(AEA))

#### FRE
Outliers <- function(x) (x <= quantile(x, .75) + (1.5 * IQR(x)))
fre1 <- data %>%
  subset (group == "FRE") %>%
  filter_at(
    c(
      'AEA',
      'AG2',
      'OEA',
      'PEA'
    ),
    Outliers
  )

summarise(fre1, min_AEA =  min(AEA), avg_AEA =mean(AEA), max_AEA = max(AEA),
          med_AEA = median(AEA), IQR_AEA = IQR(AEA))

#### ARE
Outliers <- function(x) (x <= quantile(x, .75) + (1.5 * IQR(x)))
are1 <- data %>%
  subset (group == "ARE") %>%
  filter_at(
    c(
      'AEA',
      'AG2',
      'OEA',
      'PEA'
    ),
    Outliers
  )

summarise(are1, min_AEA =  min(AEA), avg_AEA =mean(AEA), max_AEA = max(AEA),
          med_AEA = median(AEA), IQR_AEA = IQR(AEA))

#### IBD
Outliers <- function(x) (x <= quantile(x, .75) + (1.5 * IQR(x)))
ibd1 <- data %>%
  subset (group == "IBD") %>%
  filter_at(
    c(
      'AEA',
      'AG2',
      'OEA',
      'PEA'
    ),
    Outliers
  )

summarise(ibd1, min_AEA =  min(AEA), avg_AEA =mean(AEA), max_AEA = max(AEA),
          med_AEA = median(AEA), IQR_AEA = IQR(AEA))

healthy1
fre1
are1
ibd1



newdata <- rbind(healthy1, fre1, are1, ibd1)
##### Boxplot multipli....
library(reshape2)
library(ggpubr)

dati_reshaped2 = melt(newdata, id.var ="group") #riformatto il dataset in base ai fattori presenti nella colonna "group"
dati_reshaped2

#COMANDO per selezionare le variabili di interesse:
newdati <- dati_reshaped2[which(dati_reshaped2$variable == "AEA" |
                              dati_reshaped2$variable == "AG2" |
                              dati_reshaped2$variable == "OEA"  |
                              dati_reshaped2$variable == "PEA"),  ]

taglia = 12 # qui stabilisco la dimensione dei font da usare nei boxplot; con pochi boxplot usare 36

newdati$value <- as.numeric(newdati$value)
newdati$group <- as.factor(newdati$group)
glimpse(newdati)

ggboxplot(
  newdati,                               # dataset
  x = "group",                 # Variabile indipendente 
  y = "value",                  # Variabile dipendente
  facet.by = "variable", # Se più di una Variabile dipendente.... chiarire meglio...
  scales ="free", col ="black", 
  fill ="group" # colora i boxplot in base alla categoria presente nella colonna group
  #, palette = "npg"     # Colorazione secondo Nature
) +
  geom_jitter(width=0.0, size = 3, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha varia la trasparenza dei punti (1: opaco)
  stat_compare_means(
    comparisons = list(c("healthy", "FRE")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
    label = "p.signif", size = 12, vjust = 0.5) + #label: significatività: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x = "Group",                       #nome delle ascisse
       y = "Relative expression") + #nome delle ordinate
  theme(text = element_text(size=taglia, color = "black", face="bold", family = "Arial"), #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=taglia, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=taglia, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=taglia, face="bold", color="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=taglia, face="bold", color="black"), # Lettering titolo asse y
        legend.position = "none"
  )
