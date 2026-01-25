#ANOVA a due vie ds NO articolo Lucia
### 17 Febbraio 2023

#### LIBRERIE
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(emmeans)

#options(scipen=999) #  Esprime i numeri in formato fisso
options(scipen=0). ### not scientifica
sizeT = 12         #  Dimensione del font del testo nel grafico


# Caricare un dataset classico, con ogni variabile per colonna
dati <- data_set_NO2_production_4_

head(dati)

dati$Genotype <- as.factor(dati$Genotype)
dati$Treatment <- as.factor(dati$Treatment)
head(dati)

## Se occorre rinominare il nome di un livello di un vettore categoriale (es., GROUP)
dati$Treatment <- recode_factor(dati$Treatment, "LPS_KT172" = "LPS/KT172")


#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Treatment <- ordered(dati$Treatment, levels = c("CTRL", "KT172", "LPS", "LPS/KT172"))

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
dati$Genotype <- ordered(dati$Genotype, levels = c("WT", "Tg"))

#### Se serve Selezionare un fattore di un certo gruppo
#dati_8 <- subset(dati, Age == "8")

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
#dati$Treatment <- ordered(dati$Treatment, levels = c("Veh", "URB"))

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati$Genotype <- ordered(dati$Genotype, levels = c("WT", "Tg"))


#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare un fattore di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype == "Tg")

### ANALISI dati di "Fear conditioning"
###### Statistica descrittiva sui diversi gruppi comprese le età
dati %>%
  group_by(Genotype, Treatment) %>%    #### Posso selezionare uno, due o più vettori categorici.... (factors: …, ecc.)
  summarise(
    count = n(),
    median = median(NO, na.rm = TRUE),   #### NO il nome della variabile da analizzare
    mean = mean(NO, na.rm = TRUE),          #### 
    sd = sd(NO, na.rm = TRUE),                       #### 
    CV= sd/mean*100  #### coefficiente di variazione, dato che non c’era si può definirlo con una formula…
  )


##### ANALISI del gruppo a 8 mesi di età
#### Identifica outlier
dati %>%
  group_by(Genotype, Treatment) %>%
  identify_outliers(NO)


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
dati %>%
  group_by(Genotype, Treatment)%>%
  shapiro_test(NO)
### INTERPRETAZIONE: If the score were normally distributed (p > 0.05) for each cell, we can assume normality of the data.

### Create QQ plots for each cell of design:
ggqqplot(dati, "NO", ggtheme = theme_bw()) +
  facet_grid(Genotype ~ Treatment)
### INTERPRETAZIONE: If all the points fall approximately along the reference line, for each cell, we can assume normality of the data.

# Homogeneity of variance assumption (Test di Levene)
dati %>% levene_test(NO ~ Genotype*Treatment)

### INTERPRETAZIONE: If the Levene’s test is not significant (p > 0.05), we can assume the homogeneity of variances in the different groups.
### Se il test è appena significativo... 

#### Computazione modello ANOVA a due vie per design bilanciati
res.aov <- dati_8 %>% anova_test(FC ~ Genotype*Treatment)
res.aov



#dati_8$Treatment <- ordered(dati$Treatment, levels = c("Veh", "URB"))

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati_8$Genotype <- ordered(dati$Genotype, levels = c("WT", "Tg"))


my_comparisons <- list(c("CTRL", "LPS"), c("CTRL", "LPS/KT172"))


##### Post hoc comparisons entro il fattore "Treatment"
stat.test <- dati %>% 
  group_by(Genotype) %>%
  emmeans_test(NO ~ Treatment,
               comparisons = list(c("CTRL", "LPS"), c("LPS", "LPS/KT172")),
               p.adjust.method = "tukey") #### Alternatives can be "none", "fdr", "hochberg", "hommel", "holm", "tukey", "mvt", "dunnettx", "sidak", "scheffe", "bonferroni". For details, see stats::p.adjust() and emmeans::summary().
stat.test



##### Post hoc comparisons entro il fattore "Genotipo"
pwc <- dati %>% 
  group_by(Treatment) %>%
  emmeans_test(NO ~ Genotype, p.adjust.method = "tukey") #### Alternatives can be "none", "fdr", "hochberg", "hommel", "holm", "tukey", "mvt", "dunnettx", "sidak", "scheffe", "bonferroni". For details, see stats::p.adjust() and emmeans::summary().
pwc




#################################### Fine operazioni preliminari



############ BOXPLOT
p <- ggboxplot(
  dati,                     # dataset
  x = "Genotype",           # Variabile categorica 1 sulle ascisse
  y = "NO",                 # Nome della variabile dipendente
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
       y = "NO (μM)") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "none")
p   ### Visualizza il boxplot  

#### Con il valore del P value...
# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
  add_xy_position(x = "Genotype", dodge = 0.8)
stat.test$p.scient <- format(stat.test$p.adj, scientific = TRUE)
bxp.complex <- p + 
  stat_pvalue_manual(
    stat.test, 
    label = "P = {p}",
    #label = "P = {p.scient}", 
    tip.length = 0.01, y.position = c(96, 40),
  )
# 2. Add stat.test2
# Add more space between brackets using `step.increase`
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment")
pwc$p.scient <- format(pwc$p.adj, scientific = TRUE)
bxp.complex <- bxp.complex + 
  stat_pvalue_manual(
    pwc, 
    #   color = "Treatment", 
    step.group.by = "Treatment", 
    #   label = "P = {p}",
    label = "P = {p.scient}",
    tip.length = 0.01, step.increase = 0.2, y.position = c(101, 107)
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 3. Display the plot
bxp.complex 


#### Con asterischi
# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
  add_xy_position(x = "Genotype", dodge = 0.8)
bxp.complex <- p + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0.01, y.position = c(58, 58, 23, 23),
)
bxp.complex 

# 2. Add stat.test2
# Add more space between brackets using `step.increase`
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment")
bxp.complex <- bxp.complex + 
  stat_pvalue_manual(
    pwc, 
    color = "Treatment", 
    step.group.by = "Treatment",
    tip.length = 0.01, step.increase = 0.2, y.position = c(26, 36, 62 , 65)
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 3. Display the plot
bxp.complex 




#### Con asterischi
# Add p-values of `stat.test` and `stat.test2`
# 1. Add stat.test
stat.test <- stat.test %>%
  add_xy_position(x = "Genotype", dodge = 0.8)
bxp.complex <- p + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0.01, y.position = c(96, 40),
)
# 2. Add stat.test2
# Add more space between brackets using `step.increase`
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment")
bxp.complex <- bxp.complex + 
  stat_pvalue_manual(
    pwc, color = "Treatment", step.group.by = "Treatment",
    tip.length = 0, step.increase = 0.1, "p.adj.sig"
  )+
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 3. Display the plot
bxp.complex 



pwc





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
  stat_compare_means(aes(group=Treatment), method= "anova", method.args = list(var.equal = TRUE),      ###### se sono solo due gruppi: t.test
                     ### label.y=25          ### colloca il titolo del test e il suo valore globale all'ordinata 25 
                     label.y.npc = c(-0.1) #### serve per nascondere il titolo del test e il suo valore 
  )+ #### Test parametrico
  geom_signif(comparisons = list(c("WT", "Tg")), map_signif_level=TRUE,
              tip_length=0.01, y_position = 100, textsize = 4)+
  geom_signif(comparisons = list(c("Veh", "URB")), map_signif_level=TRUE,
              #annotations = c("", ""),
              y_position = c(95, 55), tip_length=0.01, textsize = 4
              # xmin=c(0.1,1.75), xmax=c(1.25,2.25),
              # tip_length=0
  )+
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



