######## CORRELAZIONE TRA VARIABILI NUMERICHE
#######
####### Versione 1.0
####### 10 giugno 2021



# Correlations with significance levels
library(Hmisc)
library(FactoMineR)


#### Importo database standard
dati <-  PLASMA_ECS_level_Foglio1

#### Se occorre rinominare le colonne
dati  <-  dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("MMSE" = "MMSE (on 30)")
#### Se serve selezionare un certo fattore
dati2 <- subset(dati, GROUP == "AD MILD")

#### Creo un dataset con solo le variabili numeriche da correlare
x <- dati2[-9, c(7, 13:16)]
print(x)

options(scipen=999)#permit decimal values
x <- as.matrix.data.frame(x)#trasformo il data set in un data frame

options(digits = 8)
rcorr(x, type="pearson") # type can be pearson (variabili distribuite in modo normale) or spearman (per variabili che non superano il test di normalità)

#Per esportare i dati
mydata.rcorr = rcorr(as.matrix(x))
mydata.rcorr

mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

#per esportare i risultati
#export into a csv file nella cartella: Dataset
write.infile(mydata.coeff, "coefficienti_mild.csv", sep=";", append = FALSE, nb.dec=4)
write.infile(mydata.p, "P-value_mild.csv", sep=";", append = FALSE, nb.dec=5)



