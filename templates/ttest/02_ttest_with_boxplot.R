##### T test box plot e analisi
### Versione: 20 gennaio 2023


#### LIBRERIE
library(ggpubr)
library(dplyr)

# Caricare un dataset classico, con ogni variabile per colonna
dati_completi <- DATASET_DEMETRA_PLASMA_CONIUGATI_Solo_gli_analizzati_in_massa

head(dati_completi)


#Creare tre vettori di identica lunghezza
Genotype <- c("wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "wt", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg", "tg")
AEA <- c(1.2, 1.5, 1.7, 20.8, 21.3, 30)
AG2 <- c(75, 54, 89, 0.01, 0.05, 0.04)

#creare una dataframe che conserva i valori su righe individuali
table <- data.frame(I(Genotype), AEA, AG2)
print(table)


#Creare due vettori di identica lunghezza
X <- c("Ctrl", "Ctrl", "Ctrl", "Ctrl", "Ctrl", "Ctrl", "Treat","Treat","Treat","Treat","Treat","Treat")
Y <- c(12.207, 16.869, 25.050, 22.429, 8.456, 20.589, 11.074, 9.686, 12.064, 9.351, 8.182, 6.642)
table <- data.frame(I(X), Y)
print(table)

options(scipen=999)     #  Esprime i numeri in formato fisso
sizeT <- 12                      #  Dimensione del font del testo nel grafico

dati_completi <- table
############################# OPERAZIONI PRELIMINARI

# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

#### Se serve modificare la tipologia del vettore categorico (Group)
dati_completi$X <- as.factor(dati_completi$X) ### da “chr” a “factor” 

#### Se serve scelgo le colonne (vettori) che mi servono
#dati_completi <-dati_completi[, c(3:9)] ### tolgo ASSAY ID e ID
head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati_completi$Genotype <- ordered(dati_completi$Genotype, levels = c("Wt", "Tg"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere osservazioni di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare osservazioni di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

#################################### Fine operazioni preliminari

dati <- dati_completi



######### Boxplot NAGly
p <- ggboxplot(
  dati,                   # dataset
  x = "X",            # Variabile indipendente 
  y = "Y",            # Nome della variabile dipendente
#  facet.by = "Timepoint",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="X" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  stat_compare_means(method="t.test", method.args = list(var.equal = TRUE), label.y=30)+ #### Se posso usare il test parametrico
  # stat_compare_means(method= "anova", label.y=25)+ #### Se posso usare il test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=0.5)+ #### Test non-parametrico
  stat_compare_means(method = "t.test",
                     method.args = list(var.equal = TRUE),
                     comparisons = list(c("Ctrl", "Treat")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                         ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     #p.adjust.method = "bonf",  #####holm, bonf....
                     label.y = c(27),   ###### se ci sono più graffette, si può selezionare le loro posizione c(y1, y2, ...)
                     label = "p.format", 
                     #label = "p.signif",
                     size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Response") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot

test <- t.test(data = dati, Y ~ X,
               var.equal = TRUE, 
               paired = FALSE,
               alternative = "two.sided",
               p.adjust.methods="holm",
               conf.level = 0.95
)
test



?stat_compare_means
