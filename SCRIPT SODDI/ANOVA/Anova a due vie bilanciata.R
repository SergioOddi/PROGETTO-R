#ANOVA a due vie disegno bilanciato
### 8 Febbraio 2023

#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(emmeans)

options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 12         #  Dimensione del font del testo nel grafico


# Caricare un dataset classico, con ogni variabile per colonna
dati <- Dati_test_cognitivi_PRIN_2015_xlsx_Dataset

head(dati)

dati$Genotype <- as.factor(dati$Genotype)
dati$Treatment <- as.factor(dati$Treatment)
dati$Age <- as.factor(dati$Age)
head(dati)




############################# OPERAZIONI PRELIMINARI
# Se serve rinominare le colone
#dati_completi <- dati_completi %>% rename("2AG" = "2-AG") ### NB: il primo termine dell'eq. sosituisce il secondo
#dati_completi <- dati_completi %>% rename("Genotype_Group" = "ID", "Genotype" = "Genotype") ### NB: il primo termine dell'eq. sosituisce il secondo
#head(dati_completi)

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
#dati_8$Genotype <- recode_factor(dati_8$Genotype, "aWT" = "WT")
#                                      , "nome da sostituire 2" = "nome corretto 2" )

### Se serve modificare la tipologia del vettore categorico (Group)
#dati_completi$Group <- as.factor(dati_completi$Group)

#### Se serve selezionare certe colonne (vettori)
#dati_completi = dati_completi[, c(3:10)]
#head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Treatment <- ordered(dati$Treatment, levels = c("Veh", "URB"))

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Genotype <- ordered(dati$Genotype, levels = c("WT", "Tg"))

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


#### Se serve Selezionare un fattore di un certo gruppo
dati_8 <- subset(dati, Age == "8")


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



###################################################
##### Post hoc comparisons entro il fattore "Treatment"

### Per limitare il numero di confronti...
### my_comparisons1 <- list(c("Veh", "LPS"), c("LPS", "LPS/"))


stat.test <- dati_8 %>% 
  group_by(Genotype) %>%
  emmeans_test(FC ~ Treatment,
               comparisons = NULL,
               p.adjust.method = "tukey"        #### Alternatives can be "none", "fdr", "hochberg", "hommel", "holm", "tukey", "mvt", "dunnettx", "sidak", "scheffe", "bonferroni". For details, see stats::p.adjust() and emmeans::summary().
               ) 
stat.test

### Per limitare il numero di confronti...
### my_comparisons2 <- list(c("Veh", "LPS"), c("LPS", "LPS/"))

##### Post hoc comparisons entro il fattore "Genotipo"
pwc <- dati_8 %>% 
  group_by(Treatment) %>%
  t_test(FC ~ Genotype,
         comparisons = NULL) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
pwc


#################################### Fine operazioni preliminari



############ BOXPLOT
p <- ggboxplot(
  dati_8,                     # dataset
  x = "Genotype",           # Variabile categorica 1 sulle ascisse
  y = "FC",                 # Nome della variabile dipendente
  color = "black",
  #order =c("WT", "Tg"),     # per ordinare i fattor
  #facet.by="Treatment",  ### separa in grafici diversi i fattori della variabile "Treatment"
  scales ="free",           # Rende automatica la scelta del range numerico dell'asse y
  fill ="Treatment",        # colora i boxplot in base alla categoria presente nella colonna "Group"
  palette = "npg",          # Colorazione secondo Nature
  repel = TRUE,
  outlier.shape = 19,       # Comando per non visualizzare gli outlier: NA; per vedere gli outlier: 19 (compaiono come punti con un cerchio grigio al centro
  add="dotplot",
  # add = c("mean", "mean_sd", "dotplot"),
  add.params = list(dotsize = 0.6, alpha = 1)
)+
  stat_summary(fun.y=mean, geom="point", shape=3, size=4, aes(group=Treatment), color="black", position=position_dodge(.8))+
  labs(title="",                          #se serve il titolo
       x = "Genotype",                    #nome asse delle ascisse
       y = "FC") +                        #nome asse delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none") #none, left, bottom, 
p   ### Visualizza il boxplot  



#### Boxplot con barre di significatività e il valore numerico del P value...
# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
add_xy_position(x = "Genotype", dodge = 0.8)
stat.test$p.scient <- format(stat.test$p.adj, scientific = TRUE)
bxp.complex <- p + 
  stat_pvalue_manual(stat.test, 
 # label = "P = {p}",
  label ="P = {scales::pvalue(p.adj)}",
  #label = "P = {p.scient}", 
  tip.length = 0.01
#  , bracket.shorten = c(0.025, 0, 0.025)
  , y.position = c(50, 100)
)
# 2. Add stat.test2
# Add more space between brackets using `step.increase`
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment", dodge = 0.8)
pwc$p.scient <- format(pwc$p.adj, scientific = TRUE)
bxp.complex <- bxp.complex +
  stat_pvalue_manual(
    pwc, 
    color = "Treatment", 
    step.group.by = "Treatment", 
#   label = "P = {p}",
#   label = "P = {p.scient}",
     label ="P = {scales::pvalue(p.adj)}",
    tip.length = 0.01, step.increase = 0.2
, digits = 5
, y.position = c(110, 120)
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 3. Display the plot
bxp.complex

#### Boxplot con barre di significatività e asterischi
# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
  add_xy_position(x = "Genotype", dodge = 0.8)
bxp.complex <- p + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0.01, 
  y.position = c(40, 100)
)
# 2. Add stat.test2
# Add more space between brackets using `step.increase`
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment")
bxp.complex <- bxp.complex + 
  stat_pvalue_manual(
    pwc, 
    fill = "Treatment",
    color = "Treatment", 
    step.group.by = "Treatment",
    tip.length = 0.01, step.increase = 0.2, 
    y.position = c(105, 110)
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 3. Display the plot
bxp.complex 





