####### ANOVA a una via


############## ANOVA a una via
##### Se le assunzioni di omogeneità delle varianze e di normalità dei dati sono state superatem si procede al test ANOVA

### carico dataset standard
dati_completi = mRNA_Napoli_xlsx_Adiposo  # dataset classico, con ogni variabile per colonna
  
  #### Nel caso occorra rimuovere outlier e/o gruppi specifici
  #dati_completi = dati_completi[-4, ]
  #### Rimuovo le osservazioni della prima tornata
  #dati_completi <- subset(dati_completi, Tornata != "Prima")
  
dati_completi <- dati_completi[, c(2:5, 11)] #Prendo solo la colonna Dieta e le colonne con le variabili

#rename columns
dati_completi <- dati_completi %>% rename("Group" = "Dieta")
head(dati_completi)

dati_completi$Group <- as.factor(dati_completi$Group)

#reshaping del dataset
dati_reshaped = melt(dati_completi, id.var ="Group") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
dati_reshaped

#### QUI SELEZIONO una variabile alla volta
df <- subset(dati_reshaped, variable == "MAGL")

df$Group <- as.factor(df$Group)

#If the levels are not automatically in the correct order, re-order them as follow:
#df$Group  <- ordered(df$Group ,
#                         levels = c("1", "2", "3"))

#rename columns
df <- df %>% rename("Response" = "value", "Condition" = "Group")



############## ANOVA a una via
##### Se le assunzioni sono state superate si procede al test ANOVA

#CASO 1: le varianze sono omogenee
oneway.test(Response ~   # nome del vettore con i valori (variabile dipendente)
              Condition,   # nome del vettore con i FATTORI (variabile indipendente)
            data = df, var.equal = TRUE)

#IN ALTERNATIVA CASO 1:
#NB: La funzione "aov" si usa per "design bilanciati" (in cui tutti i gruppi hanno la stessa numerosità)
#NB2: serve per computare il Tukey test
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

########SECONDA MODALITA': The function "pairewise.t.test()" can be also used to calculate pairwise comparisons between group levels 
#with corrections for multiple testing.

pairwise.t.test(df$Response,             # definisce la colonna del df con la variabile dipendente 
                df$Condition,                         # definisce la colonna del df con la variabile indipendente 
                p.adjust.method = "holm")   # definisce il metodo di correzione; ce ne sono  altri: "hochberg","hommel","bonferroni","BH","BY","fdr","none"
