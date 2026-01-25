#VALUTAZIONE DELLE ASSUNZIONI per il test ANOVA a una via
#Novembre 2020

library("dplyr")
library("stats")
library("car")

df <- test_anova
df <- df[, 1:2]

df$Condition <- as.factor(df$Condition)

#If the levels are not automatically in the correct order, re-order them as follow:
df$Condition  <- ordered(df$Condition ,
                           levels = c("Sugar", "A little sugar", "No sugar"))

print(df)


####Parametri statistici
group_by(df, Condition) %>%
  summarise(
    count = n(),
    mean = mean(Response, na.rm = TRUE),
    sd = sd(Response, na.rm = TRUE)
    )




########## Testing the homogeneity of variance assumption
# The classical one-way ANOVA test requires an assumption of equal variances for all groups. 

# Levene’s test (valuta se i gruppi hanno "varianza uguale" aka "homoscedasticity" (nella library car)

leveneTest(Response ~   # nome del vettore con i valori (variabile dipendente)
           Condition,   # nome del vettore con i FATTORI (variabile indipendente)
           data = df)   # il dataset

#Interpretazione risultati del test: In our example, the homogeneity of variance assumption 
# turned out to be fine: "the Levene test is not significant" P value è uguale a 0.6186, i.e., è > 0.05.
#In pratica, nella formula del modello ANOVA occorre inserire l'istruzione "var.equal = TRUE"

#ATTENZIONE! How do we save our ANOVA test, in a situation where the homogeneity of variance assumption is violated?
#Nel caso le varianze non dovessero essere omogenee occorre correggere il test ANOVA usando la correzione di Welch
#Quindi nella formula del modello ANOVA occorre inserire l'istruzione "var.equal = FALSE"



######### Checking the normality assumption
#un'altra importanza assunzione per poter effettuare il test anova è che i residui della variabile dipendente siano distributi in modo
#normale

# Primo test. Plot della normalità della distribuzione dei residui

#  Prima computo the analysis of variance
res.aov <- aov(Response ~ 
                 Condition, 
               data = df)

#PLOT della distribuzione dei residui
plot(res.aov, 2)

#Interpretazione: In the plot, the quantiles of the residuals are plotted against the quantiles of the normal distribution. A 45-degree reference line is also plotted.
#Se i residui sono distributi in modo normale, la distribuzione dovrebbe approssimarsi su una linea retta.

# Secondo test: test di Shapiro-Wilk per verificare l'ipotesi della normalità dei residui ANOVA
#Estrazione dei residui
aov_residuals <- residuals(object = res.aov)

# Shapiro-Wilk normality test
shapiro.test(x = aov_residuals)
#Interpretazione se il P value del test è > di 0.05, allora non ci sono indizi di violazione della normalità dei residui e si può procedere con il test ANOVA


############## ANOVA a una via
##### Se le assunzioni sono state superate si procede al test ANOVA

#CASO 1: le varianze sono omogenee
oneway.test(Response ~   # nome del vettore con i valori (variabile dipendente)
              Condition,   # nome del vettore con i FATTORI (variabile indipendente)
            data = df, var.equal = TRUE)

#IN ALTERNATIVA CASO 1:
#NB: La funzione "aov" si usa per "design bilanciati" (in cui tutti i gruppi hanno la stessa numerosità)
#NB2: serve per computare il Tukey Test
res.aov <- aov(Response ~ 
               Condition, 
               data = df)

#CASO 2: le varianze non sono omogenee
oneway.test(Response ~   # nome del vettore con i valori (variabile dipendente)
            Condition,   # nome del vettore con i FATTORI (variabile indipendente)
            data = df, var.equal = FALSE)

#CONFRONTI TRA i gruppi
#nel caso il test ANOVA sia positivo (P value <0.05)
#It’s possible to perform multiple pairwise-comparison, to determine if the mean difference between specific pairs of group are statistically significant.

########PRIMA MODALITA': Tukey multiple pairwise-comparison
TukeyHSD(res.aov)

########Seconda MODALITA': The function "pairewise.t.test()" can be also used to calculate pairwise comparisons between group levels 
#with corrections for multiple testing.
pairwise.t.test(df$Response,   # definisce la colonna del df con la variabile dipendente 
                df$Condition,  # definisce la colonna del df con la variabile indipendente 
                p.adjust.method = "holm") # definisce il metodo di correzione; ce ne sono  altri: "hochberg","hommel","bonferroni","BH","BY","fdr","none"



##### TEST NON PARAMETRICO
######NEL CASO IN CUI, per un evidente fallimento dei test di normalità e di omogeneità delle varianze, NON SI POTESSE USARE IL MODELLO PARAMETRICO (ANOVA), 
#si deve usare il test non parametrico di Kruskal-Wallis:

kruskal.test(Response ~   # nome del vettore con i valori (variabile dipendente)
             Condition,   # nome del vettore con i FATTORI (variabile indipendente)
             data = df)

#Multiple pairwise-comparison between groups
#Se dall' output del Kruskal-Wallis test, c'è una diff. sign. fra i gruppi
#It’s possible to use the function pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(df$Response, 
                     df$Condition,
                     p.adjust.method = "holm")

