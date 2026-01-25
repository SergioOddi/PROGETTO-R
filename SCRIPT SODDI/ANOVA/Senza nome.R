#ANOVA a due vie disegno bilanciato
### 8 Febbraio 2023

#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)


options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 12         #  Dimensione del font del testo nel grafico


# Caricare un dataset classico, con ogni variabile per colonna
dati <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset

head(dati)

dati$Genotype <- as.factor(dati$Genotype)
dati$Treatment <- as.factor(dati$Treatment)
dati$Age <- as.factor(dati$Age)
head(dati)

#### Se serve Selezionare un fattore di un certo gruppo
dati_8 <- subset(dati, Age == "8")

############################# OPERAZIONI PRELIMINARI

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
#dati_completi = dati_completi[, c(3:10)]
#head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Treatment <- ordered(dati$Treatment, levels = c("Veh", "URB"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

### ANALISI dati di "Fear conditioning"
###### Statistica descrittiva sui diversi gruppi comprese le età
dati %>%
  group_by(Age, Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: …, ecc.)
  summarise(
    count = n(),
    median = median(FC, na.rm = TRUE),   #### FC il nome della variabile da analizzare
    mean = mean(FC, na.rm = TRUE),          #### 
    sd = sd(FC, na.rm = TRUE),                       #### 
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )


##### ANALISI del gruppo a 8 mesi di età
#### Identifica outlier
dati_8 %>%
  group_by(Genotype, Treatment) %>%
  identify_outliers(FC)


#### Check normality assumption by analyzing the model residuals. QQ plot and Shapiro-Wilk test of normality are used.
# Build the linear model
model  <- lm(FC ~ Genotype*Treatment,
             data = dati_8)

# Create a QQ plot of residuals
ggqqplot(residuals(model))

### INTERPRETAZIONE: Se nel QQ plot the points fall approximately along the reference line, we can assume normality.

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

### INTERPRETAZIONE: se il p value è maggiore di 0.05, possiamo considerare che i dati siano distribuiti normalmente

# Check normality assumption by groups. Computing Shapiro-Wilk test for each combinations of factor levels:
dati_8 %>%
  group_by(Genotype, Treatment)%>%
  shapiro_test(FC)
### INTERPRETAZIONE: If the score were normally distributed (p > 0.05) for each cell, we can assume normality of the data.

### Create QQ plots for each cell of design:
ggqqplot(dati_8, "FC", ggtheme = theme_bw()) +
  facet_grid(Genotype ~ Treatment)
### INTERPRETAZIONE: If all the points fall approximately along the reference line, for each cell, we can assume normality of the data.

# Homogeneity of variance assumption (Test di Levene)
dati_8 %>% levene_test(FC ~ Genotype*Treatment)

### INTERPRETAZIONE: If the Levene’s test is not significant (p > 0.05), we can assume the homogeneity of variances in the different groups.
### Se il test è appena significativo... 

#### Computazione modello ANOVA a due vie per design bilanciati
res.aov <- dati_8 %>% anova_test(FC ~ Genotype*Treatment)
res.aov



#################################### Fine operazioni preliminari
p <- ggboxplot(
  dati_8,                     # dataset
  x = "Genotype",           # Variabile categorica 1 sulle ascisse
  y = "FC",                 # Nome della variabile dipendente
  color = "black",
  order =c("WT", "Tg"),     # per ordinare i fattori
  scales ="free",           # Rende automatica la scelta del range numerico dell'asse y
  fill ="Treatment",        # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg",          # Colorazione secondo Nature
  repel = TRUE,
  outlier.shape = 19,       # Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro
  add="dotplot",
  # add = c("mean", "mean_sd", "dotplot"), 
  add.params = list(dotsize = 0.3, alpha = 1))+
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Treatment), color="black", fill="red", position=position_dodge(.8))+
  labs(title="",                           #se serve il titolo
       x = "Genotype",                        #nome delle ascisse
       y = "FC") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")
p   ### Visualizza il boxplot  





res.aov <- dati %>% anova_test(NO ~ Genotype * Treatment)
res.aov

library(emmeans)
pwc <- dati %>% 
  group_by(Genotype) %>%
  emmeans_test(NO ~ Treatment, p.adjust.method = "bonferroni") 
pwc

pws <- dati %>% 
  group_by(Treatment) %>%
  emmeans_test(NO ~ Genotype, p.adjust.method = "bonferroni") 
pws



