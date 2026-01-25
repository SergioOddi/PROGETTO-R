#######  mBOXPLOT no-reshaping Confronto timepoint diversi dello stesso gruppo
####### 17 dicembre 2022

#### LIBRERIE
library(ggpubr)
library(dplyr)

options(scipen=999) #  Esprime i numeri in formato fisso
sizeT <- 12         #  Dimensione del font del testo nel grafico


# Caricare un dataset classico, con ogni variabile per colonna
dati_completi <- DATASET_DEMETRA_PLASMA_CONIUGATI_Senza_outlier_2
head(dati_completi)

#  ID     Genotype Group   Nitrotyrosine     Actin      Ratio
# <chr>   <ord>    <chr>        <dbl>       <dbl>       <dbl>
#1 WT Veh Wt       PLACEBO      80800       41000        1.97
#2 WT Veh Wt       PLACEBO      57700       47400        1.22
#3 TG Veh Tg       PLACEBO      112000      40500        2.77

############################# OPERAZIONI PRELIMINARI

# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

#### Se serve modificare la tipologia del vettore categorico (Group)
dati_completi$Group <- as.factor(dati_completi$Group)

#### Se serve scelgo le colonne (vettori) che mi servono
dati_completi = dati_completi[, c(3:9)]
head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati_completi$Genotype <- ordered(dati_completi$Genotype, levels = c("Wt", "Tg"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere osservazioni di un certo gruppo
#dati_completi <- subset(dati_completi, Group != "controllo")

#### Se serve Selezionare osservazioni di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

#################################### Fine operazioni preliminari

dati <- dati_completi



######### Boxplot NAGly
p <- ggboxplot(
  dati,                   # dataset
  x = "Timepoint",            # Variabile indipendente 
  y = "NAGLY",            # Nome della variabile dipendente
  facet.by = "Group",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Timepoint" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  # stat_compare_means(method= "anova", label.y=25)+ #### Se posso usare il test parametrico
  stat_compare_means(method= "kruskal.test", label.y=25)+ #### Test non-parametrico
  stat_compare_means(comparisons = list(c("Timepoint 0", "Timepoint 1"), c("Timepoint 0", "Timepoint 2"), c("Timepoint 1", "Timepoint 2")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     #method = "t.test",    ###### confronto tra due gruppi
                     method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  #####holm, bonf....
                     label = "p.signif.adj", size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "NAGly") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot


######### Boxplot NASer
p <- ggboxplot(
  dati,                   # dataset
  x = "Timepoint",            # Variabile indipendente 
  y = "NASER",            # Nome della variabile dipendente
  facet.by = "Group",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Timepoint" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  stat_compare_means(method= "anova", label.y=25)+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("Timepoint 0", "Timepoint 1"), c("Timepoint 0", "Timepoint 2"), c("Timepoint 1", "Timepoint 2")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test",    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  #####holm, bonf....
                     label = "p.signif.adj", size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "NASer") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot

######### Boxplot NGABA
p <- ggboxplot(
  dati,                   # dataset
  x = "Timepoint",            # Variabile indipendente 
  y = "NGABA",            # Nome della variabile dipendente
  facet.by = "Group",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Timepoint" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  stat_compare_means(method= "anova", label.y=25)+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=20)+
  stat_compare_means(comparisons = list(c("Timepoint 0", "Timepoint 1"), c("Timepoint 0", "Timepoint 2"), c("Timepoint 1", "Timepoint 2")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test",    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "holm",  #####holm, bonf....
                     label = "p.signif.adj", size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "NGABA") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot

######### Boxplot NALA
p <- ggboxplot(
  dati,                   # dataset
  x = "Timepoint",            # Variabile indipendente 
  y = "NALA",            # Nome della variabile dipendente
  facet.by = "Group",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Timepoint" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  # stat_compare_means(method= "anova", label.y=25)+ #### Test parametrico
  stat_compare_means(method= "kruskal.test", label.y=15)+
  stat_compare_means(comparisons = list(c("Timepoint 0", "Timepoint 1"), c("Timepoint 0", "Timepoint 2"), c("Timepoint 1", "Timepoint 2")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     #method = "t.test",    ###### confronto tra due gruppi
                     method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  #####holm, bonf....
                     label = "p.signif.adj", size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "NAla") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot


######### Boxplot NADA
p <- ggboxplot(
  dati,                   # dataset
  x = "Timepoint",            # Variabile indipendente 
  y = "NADA",            # Nome della variabile dipendente
  facet.by = "Group",  # Se più di un gruppo
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Timepoint" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = NA   ### Comando per non visualizzare gli outlier; per vedere gli outlier: outlier.shape = 19
  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  , add.params =list(size = 0.6)
) +
  #geom_jitter(width=0.0, size = 2, alpha = 0.6) + #caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  #facet_wrap(~Group)+
  stat_compare_means(method= "anova", label.y=25)+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=20)+ #### Test non parametrico
  # stat_compare_means(method= "anova", label.y=25)+ #### Test parametrico
  stat_compare_means(comparisons = list(c("Timepoint 0", "Timepoint 1"), c("Timepoint 0", "Timepoint 2"), c("Timepoint 1", "Timepoint 2")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test",    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  #####holm, bonf....
                     label = "p.signif.adj", size = 4, vjust = 0.2) + #label: significatività: decide se mostrare il numero o l'asterisco: con p.signif: mostra l'asterisco ### size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "NADA") +                 #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot









