#BOXPLOT MULTIPLI senza reshaping del dataset
### versione 27 GENNAIO 2023

#### Nota bene: prima di eseguire questo script occorre decidere che tipo di confronto si può fare sui gruppi: parametrico (t-test o ANOVA) o non parametrico (kruskal.test) e se la varianza è omogenea o meno

#######  BOXPLOT MULTIPLI senza reshaping del dataset
####### 20 gennaio 2023

#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)

sizeT = 12

dati_completi <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset
### Se occorre cambiare il nome a un vettore
#dati <- dati %>% rename("Group" = "GROUP") 
#dati <- dati %>% rename("Timepoint" = "TIMEPOINT") 

head(dati_completi)

#### Se serve modificare la tipologia del vettore categorico (Group)
dati_completi$Group <- as.factor(dati_completi$Group)
dati_completi$Genotype <- as.factor(dati_completi$Genotype)
dati_completi$Treatment <- as.factor(dati_completi$Treatment)
dati_completi$Age <- as.factor(dati_completi$Age)
head(dati_completi)
############################# OPERAZIONI PRELIMINARI

# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("2AG" = "2-AG") ### NB: il primo termine dell'eq. sosituisce il secondo
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
dati_completi$Group <- recode_factor(dati_completi$Group, "WT+Veh" = "Wt+Veh"
                                      , "WT+URB" = "Wt+URB" )

dati_completi$Age <- recode_factor(dati_completi$Age, "8" = "8 months of age", "10" = "10 months of age",
                                     , "12" = "12 months of age" )




### Se serve modificare la tipologia del vettore categorico (Group)
#dati_completi$Group <- as.factor(dati_completi$Group)

#### Se serve selezionare certe colonne (vettori)
#dati_completi = dati_completi[, c(3:10)]
#head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati_completi$Genotype <- ordered(dati_completi$Genotype, levels = c("Wt", "Tg"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere un fattore di un certo gruppo
dati_completi <- subset(dati_completi, Age != "10")

#### Se serve Selezionare un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

#################################### Fine operazioni preliminari

dati <- dati_completi

######### Boxplot FC
p <- ggboxplot(
  dati,                   # dataset
  x = "Group",    # Variabile indipendente 
  y = "FC",        # Nome della variabile dipendente
  facet.by = "Age",      # Se più di un gruppo
  order =c("Wt+Veh", "Wt+URB", "Tg+Veh", "Tg+URB"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(dotsize = 0.6, fill= "white"),
  #  , add.params =list(fill= "white", dotsize = 0.8, alpha = 0.7),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "anova", method.args = list(var.equal = TRUE), 
                     ##label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("Wt+Veh", "Wt+URB"), c("Wt+Veh", "Tg+Veh"), c("Tg+Veh", "Tg+URB"), c ("Wt+URB", "Tg+URB")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     #hide.ns = T ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif.adj", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Freezing (%)") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot



######### Boxplot YM
p <- ggboxplot(
  dati,                   # dataset
  x = "Group",    # Variabile indipendente 
  y = "YM",        # Nome della variabile dipendente
  facet.by = "Age",      # Se più di un gruppo
  order =c("Wt+Veh", "Wt+URB", "Tg+Veh", "Tg+URB"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(dotsize = 0.6, fill= "white"),
  #  , add.params =list(fill= "white", dotsize = 0.8, alpha = 0.7),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "anova", method.args = list(var.equal = TRUE), 
                     ##label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("Wt+Veh", "Wt+URB"), c("Wt+Veh", "Tg+Veh"), c("Tg+Veh", "Tg+URB"), c ("Wt+URB", "Tg+URB")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     #hide.ns = T ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif.adj", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Spontaneous alternation (%)") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot


######### Boxplot NORT
p <- ggboxplot(
  dati,                   # dataset
  x = "Group",    # Variabile indipendente 
  y = "NORT",        # Nome della variabile dipendente
  facet.by = "Age",      # Se più di un gruppo
  order =c("Wt+Veh", "Wt+URB", "Tg+Veh", "Tg+URB"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(dotsize = 0.6, fill= "white"),
  #  , add.params =list(fill= "white", dotsize = 0.8, alpha = 0.7),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "anova", method.args = list(var.equal = TRUE), 
                     ##label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("Wt+Veh", "Wt+URB"), c("Wt+Veh", "Tg+Veh"), c("Tg+Veh", "Tg+URB"), c ("Wt+URB", "Tg+URB")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     #hide.ns = T ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif.adj", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Preference index (%)") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot


######### Boxplot NORT a 10 mesi di età
dati_completi <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset
### Se occorre cambiare il nome a un vettore
#dati <- dati %>% rename("Group" = "GROUP") 
#dati <- dati %>% rename("Timepoint" = "TIMEPOINT") 

head(dati_completi)

#### Se serve modificare la tipologia del vettore categorico (Group)
dati_completi$Group <- as.factor(dati_completi$Group)
dati_completi$Genotype <- as.factor(dati_completi$Genotype)
dati_completi$Treatment <- as.factor(dati_completi$Treatment)
dati_completi$Age <- as.factor(dati_completi$Age)
head(dati_completi)
############################# OPERAZIONI PRELIMINARI

# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("2AG" = "2-AG") ### NB: il primo termine dell'eq. sosituisce il secondo
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
dati_completi$Group <- recode_factor(dati_completi$Group, "WT+Veh" = "Wt+Veh"
                                     , "WT+URB" = "Wt+URB" )

dati_completi$Age <- recode_factor(dati_completi$Age, "8" = "8 months of age", "10" = "10 months of age"
                                   , "12" = "12 months of age" )

###Seleziono solo il gruppo a 10 mesi
dati_completi <- subset(dati_completi, Age == "10 months of age")
dati <- dati_completi

p <- ggboxplot(
  dati,                   # dataset
  x = "Group",    # Variabile indipendente 
  y = "NORT",        # Nome della variabile dipendente
  facet.by = "Age",      # Se più di un gruppo
  order =c("Wt+Veh", "Wt+URB", "Tg+Veh", "Tg+URB"),       # per ordinare i fattori
  scales ="free",         # Rende automatica la scelta del range numerico dell'asse y
  col ="black",           # Colore dei contorni dei boxplot 
  fill ="Group" # colora i boxplot in base alla categoria presente nella colonna "Group"
  , palette = "npg"     # Colorazione secondo Nature
  , repel = TRUE
  , outlier.shape = 19   ### Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro)
  #  , add = "dotplot"### Comando per disporre in modo simmetrico i dati (è alternativo a geom_jitter: vedi riga sotto)
  #   , add = c("mean", "mean_sd", "violin"),
  , add = c("mean", "mean_sd", "dotplot"), #### aggiunge al boxplot la media (punto nero) e la ds (il segmento sopra e sotto)
  , add.params =list(dotsize = 0.6, fill= "white"),
  #  , add.params =list(fill= "white", dotsize = 0.8, alpha = 0.7),  ### riempie di bianco i dati, alpha: rende trasparenza dei punti
) +
  #  , add = c("jitter", "mean", "mean_sd"), geom_jitter(width=0.0, size = 0.8, alpha = 0.6) +  ### per disporre i punti in modo "scatterato"
  # caratteristiche dello scatter dei dati; alpha la trasparenza dei punti (1: opaco)
  # facet_wrap(~Timepoint) +
  stat_compare_means(method= "anova", method.args = list(var.equal = TRUE), 
                     ##label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 24 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  #stat_compare_means(method= "kruskal.test", label.y=25)+
  stat_compare_means(comparisons = list(c("Wt+Veh", "Wt+URB"), c("Wt+Veh", "Tg+Veh"), c("Tg+Veh", "Tg+URB"), c ("Wt+URB", "Tg+URB")), # Qui si può decidere le coppie su cui fare la significatività statistica in base al numero di trattamenti
                     method = "t.test", method.args = list(var.equal = TRUE),    ###### confronto tra due gruppi
                     #method="wilcox.test",  ###### confronto tra due gruppi con varianza anche diversa
                     p.adjust.method = "bonf",  ##### Corregge il valore dei p value in confronti multipli; metodi: holm, bonf....
                     label = "p.signif", #### mostra asterischi e ns
                     #hide.ns = T ,  ### se posto vero, non mostra "ns"
                     #label = "p.signif.adj", # mette i valori di p value
                     size = 4, vjust = 0.2) + #size: la taglia; e vjust: la sua posizione rispetto alla graffetta 
  labs(title="",                           #se serve il titolo
       x = "Group",                        #nome delle ascisse
       y = "Preference index (%)") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none"
  )
p   ### Visualizza il boxplot




