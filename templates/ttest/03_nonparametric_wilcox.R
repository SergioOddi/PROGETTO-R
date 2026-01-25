##### TEST NON PARAMETRICO
#### 18 dicembre 2022

###### NB: NEL CASO IN CUI, per un fallimento dei test di normalità e di omogeneità delle varianze, NON SI POTESSE USARE IL MODELLO PARAMETRICO (ANOVA), 
###### si deve usare il test non parametrico di Kruskal-Wallis:

library(stats)
library(ggpubr)
library(dplyr)
library(car)
library(FSA) # Per il Dunn test
library(rcompanion) # Per il Nemenyi test 
library(DescTools)

library(reshape2)

# Caricare un dataset classico, con ogni variabile per colonna
dati_completi <- DATASET_DEMETRA_PLASMA_CONIUGATI_SOLO_GLI_ANALIZZATI
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
dati_completi$Timepoint <- as.factor(dati_completi$Timepoint)

#### Se serve scelgo le colonne (vettori) che mi servono
dati_completi = dati_completi[, c(3:9)]
head(dati_completi)

#### Se serve modificare l'ordine di comparsa dei Trattamenti/Gruppi/Genotipi
#dati_completi$Genotype <- ordered(dati_completi$Genotype, levels = c("Wt", "Tg"))

#### Se serve rimuovere oulier
#dati_completi = dati_completi[ , ]

#### Se serve rimuovere osservazioni di un certo gruppo
#dati_completi <- subset(dati_completi, Genotype != "Wt")

#### Se serve Selezionare osservazioni di un certa popolazione
dati_analizzati <- subset(dati_completi, Timepoint == "Timepoint 0")

#### Se serve scelgo le colonne (vettori) che mi servono
dati_analizzati = dati_analizzati[, -2]
head(dati_analizzati)

#reshaping del dataset
dati_reshaped = melt(dati_analizzati, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#### QUI SELEZIONO una variabile alla volta
df <- subset(dati_reshaped, variable == "NALA")

df$Group <- as.factor(df$Group)

#If the levels are not automatically in the correct order, re-order them as follow:
#df$Group  <- ordered(df$Group ,
#                         levels = c("1", "2", "3"))

#rename columns
df <- df %>% rename("Response" = "value", "Condition" = "Group")


######### TEST dell'omogeneità delle varianze. Test di Levene
leveneTest(Response ~   # nome del vettore con i valori (variabile dipendente)
             Condition,   # nome del vettore con i FATTORI (variabile indipendente)
           data = df)

#Interpretazione risultati del Levene's test: 
# (i) Se il P-value del test è > 0.05, ossia non è significativo, si può accettare che la varianza dei gruppi sia uguale
#In pratica, nella formula del modello ANOVA occorre inserire l'istruzione "var.equal = TRUE"
# (ii) Se il P-value del test è significativo (P <0.05) le varianze non sono omogenee e occorre correggere il test ANOVA usando la correzione di Welch 
#In pratica, nella formula del modello ANOVA occorre inserire l'istruzione "var.equal = FALSE"

#### TEST della normalità dei dati
######### Checking the normality assumption
#Un'altra assunzione per poter effettuare il test ANOVA è che i residui della variabile dipendente siano distributi in modo normale


######### Primo test. Plot della normalità della distribuzione dei residui
# Prima computo the analysis of variance
res.aov <- aov(Response ~ 
                 Condition, 
               data = df)

#PLOT della distribuzione dei residui
plot(res.aov, 2)

#Interpretazione: In the plot, the quantiles of the residuals are plotted against the quantiles of the normal distribution. 
#A 45-degree reference line is also plotted. 
#Se i residui sono distributi in modo normale, i punti dovrebbero addossarsi lungo una linea retta.

# Secondo test: test di Shapiro-Wilk per verificare l'ipotesi della normalità dei residui ANOVA
#Estrazione dei residui
aov_residuals <- residuals(object = res.aov)

######## Shapiro-Wilk normality test
shapiro.test(x = aov_residuals)
# Interpretazione 
# (i) se il P value del test è > di 0.05, allora non ci sono indizi di violazione della normalità dei residui e si può procedere con il test ANOVA
# (i) se il P value del test è < di 0.05, allora c'è evidenza di violazione della normalità dei residui e si deve procedere con i test non parametrici

################################################################################
######## TEST NON PARAMETRICO di KRUSKAL-WALLIS  ###############################
kruskal.test
kruskal.test(Response ~       # nome del vettore con i valori (variabile dipendente)
                     Condition,          # nome del vettore con i FATTORI (variabile indipendente)
                     data = df)          # nome del dataset

#Multiple pairwise-comparison between groups
#Se dall'output del Kruskal-Wallis test, c'è una diff. sign. fra i gruppi
#It’s possible to use the function pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(df$Response, 
                     df$Condition,
                     p.adjust.method = "holm") #bonf

pairwise.wilcox.test(df$Response, 
                     df$Condition,
                     p.adjust.method = "bonf") #bonf


### Dunn test (post-hoc test)
##### Zar (2010) states that the Dunn test is appropriate for groups with unequal numbers of observations

PT = dunnTest(Response ~ Condition, data=df, method="bh")
PT

# Can adjust p-values;
# See ?p.adjust for options

### Nemenyi test for multiple comparisons
### Zar (2010) suggests that the Nemenyi test is not appropriate for groups with unequal numbers of observations.

PT = NemenyiTest(x = df$Response,         #Vettore Risposta
                 g = df$Condition,                             # Vettore Gruppo
                 dist="tukey")
PT

### Pairwise Mann–Whitney U-tests
## Another post-hoc approach is to use pairwise Mann–Whitney U-tests. To prevent the inflation of type I error ## rates, adjustments to the p-values can be made using the p.adjust.method option to control the familywise ## error rate or to control the false discovery rate. See ?p.adjustfor details.

PT = pairwise.wilcox.test(df$Response,
                          df$Condition,
                          p.adjust.method="none")

# Can adjust p-values;
# See ?p.adjust for options

PT
