########## PCA analisi preliminare
########## I dati sono sufficienti per l'analisi in PCA?


#### Librerie
library(psych)
library(rela)
library(MASS)
library(parallel)

options(scipen=999)


dati <-  PLASMA_ECS_level_Foglio1
head(dati)


#### Se occorre rinominare le colonne
dati <- dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("Group" = "GROUP")

### Escludo qualche osservazione (soggetti: 1, 2, 38) e tengo il vettore coi gruppi e gli eCB
dati <- dati[-c(1,2,38), c(2, 13:16)]

#### Se occorre escludere qualche gruppo
dati <- subset(dati, Group != "ctrl")

pca <- dati[, -1] ### solo variabili numeriche

pca <- as.matrix.data.frame(pca)

pcacor = cor(pca)#computa la matrice di correlazione
pcacor
det(pcacor)#è il determinate della matrice di correlazione: deve essere positivo! Prima assunzione da testare per fare una PCA di successo!

pcacov=cov(pca)#computa la matrice di covarianza
pcacov
det(pcacov)

out= corr.p(cor(pca), 68, alpha=.05, adjust="bonferroni") ##### il numero tra (pca) e alpha è il numero di osservazioni (righe)
print(out)

paf.pca = paf(pca, eigcrit = 1, convcrit=.001)
summary(paf.pca)
#Il comando "paf" ci dà il valore $KMO che deve essere >0.5 (>0.7 sarebbe meglio). 

cortest.bartlett(pcacor, n=68) # dove "n"  è il numero di osservazioni (righe)
#il comando ci dà il risultato del test di Bartlett. If we obtained a very small p-value, 
#then we can safely reject the null and conclude the alternative that the matrix is not an identity matrix. 
#That is, we have evidence to suggest we have sufficient correlation in the matrix to proceed!







