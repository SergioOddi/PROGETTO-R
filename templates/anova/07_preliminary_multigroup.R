########## ANALISI preliminare scelta tra ANOVA e test non parametrici
########## V.02 revisione: 25 maggio 2021

library(stats)
library(ggpubr)
library(reshape2)
library(dplyr)
library(car)
library(FSA) # Per il Dunn test
library(rcompanion) # Per il Nemenyi test 
library(DescTools)

options(scipen=999) # esprime i numeri in formato fisso



### carico dataset standard
dati_completi = mRNA_Napoli_xlsx_Muscolare # dataset classico, con ogni variabile per colonna

#### Nel caso occorra rimuovere outlier e/o gruppi specifici
#dati_completi = dati_completi[-4, ]
#### Rimuovo le osservazioni della prima tornata
#dati_completi <- subset(dati_completi, Tornata != "Prima")

dati_completi <- dati_completi[, 3:12] #Prendo solo la colonna Dieta e le colonne con le variabili

#rename columns
dati_completi <- dati_completi %>% rename("Group" = "Dieta")
head(dati_completi)

dati_completi$Group <- as.factor(dati_completi$Group)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#### QUI SELEZIONO una variabile alla volta
df <- subset(dati_reshaped, variable == "DAGLb")

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

#Interpretazione risultati del test: 
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
#(i) se il P value del test è < di 0.05, allora c'è evidenza di violazione della normalità dei residui e si deve procedere con i test non parametrici


################################################
######## TEST NON PARAMETRICO di KRUSKAL-WALLIS

kruskal.test(Response ~                  # nome del vettore con i valori numerici (variabile dipendente)
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
## Another post-hoc approach is to use pairwise Mann–Whitney U-tests.
PT = pairwise.wilcox.test(df$Response,
                          df$Condition,
                          p.adjust.method="none")
# Can adjust p-values;
# See ?p.adjust for options
PT


