#### Analisi livelli eCB nella microglia WT e Tg  
### 6 febbraio 2023


#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)

options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 12         #  Dimensione del font del testo nel grafico


# Caricare un dataset classico, con ogni variabile per colonna
dati <- data_set_LC_MS

head(dati)

dati$Genotype <- as.factor(dati$Genotype)
dati <- dati %>% rename("AG" = "2-AG") ### NB: il primo termine dell'eq. sosituisce il secondo
head(dati)


######### Boxplot ΑΕΑ
p <- ggboxplot(
  dati,                   # dataset
  x = "Genotype",    # Variabile indipendente 
  y = "AEA",        # Nome della variabile dipendente
#  facet.by = "Timepoint",      # Se più di un gruppo
 order =c("WT", "Tg"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Genotype" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(fill= "white", dotsize = 0.8, alpha = 1),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "t.test", method.args = list(var.equal = TRUE), 
                     ## label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                      label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("WT", "Tg")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     #p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     hide.ns = F ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Genotype",                        #nome delle ascisse
       y = "AEA") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot


######### Boxplot 2-AG
p <- ggboxplot(
  dati,                   # dataset
  x = "Genotype",    # Variabile indipendente 
  y = "2-AG",        # Nome della variabile dipendente
  #  facet.by = "Timepoint",      # Se più di un gruppo
  order =c("WT", "Tg"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Genotype" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(fill= "white", dotsize = 0.8, alpha = 1),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "t.test", method.args = list(var.equal = TRUE), 
                     ## label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("WT", "Tg")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     #p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     hide.ns = F ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Genotype",                        #nome delle ascisse
       y = "2-AG") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot






