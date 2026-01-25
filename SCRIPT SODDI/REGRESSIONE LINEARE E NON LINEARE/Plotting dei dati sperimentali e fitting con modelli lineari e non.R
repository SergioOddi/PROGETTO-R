##### Plotting dati sperimentali medi e fitting dei punti
### versione 01
### 25 maggio 2021

#LIBRERIE da utilizzare
library(ggplot2)
library(ggstatsplot)
library(ggpubr)
library(rstatix)
library(reshape2)

dati <- Time_Sheet2_2_

dati <- dati[, c(1,2)]

#reshaping del dataset
dati_reshaped = melt(dati, id.var ="Time") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
df <- dati_reshaped

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
# to be summarized
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(df, #il dataset
                    varname="value", #nome della varibile risposta
                    groupnames=c("Time")) #vettore dei nomi di colonne che contengono i fattori


## Alternativamente:
plot(df2$Time, df2$value,                    ## indica quali sono i punti da graficare prendendo in ordine le ascisse dal vettore x e le ordinate dal vettore y
     ylim=c(0,80),                ## definisce il range sulle ordinate
     ylab = "Fluorescence (%)",
     xlab = "Time (min)" )

#Trova i parametri cinetici della curva di Michaelis Menten tramite regressione non lineare
x <- df2$Time 
y <- df2$value   
NLR <- nls(y ~ (Vm * x)/(Km + x), 
           data = df2, 
           start = list(Km = 1, Vm = 0.2), #in base ai dati grezzi è possibile fare una prima stima dei valori di Km e Vax
           trace = TRUE) 
summary(NLR)

abline(h = coef(NLR)[2], lty = 2) #traccia l'asintodo dell'iperbole, ossia il valore della Vmax
coef(NLR)
Km = coef(NLR)[1] # Attribuisco il valore numerico alla Km
Vm = coef(NLR)[2] # Attribuisco il valore numerico alla Vm

#Disegna la curva usando i parametri stimati
curve((Vm*x)/(Km+x), from = 1, to = 20, add = TRUE, col = "red")  



NLR2 <- nls(y ~ Vm * (1-exp(-1*Km*x)), 
           data = df2, 
           start = list(Km = 0.2, Vm = 60), #in base ai dati grezzi è possibile fare una prima stima dei valori di Km e Vax
           trace = TRUE) 
summary(NLR2)

abline(h = coef(NLR2)[2], lty = 2) #traccia l'asintodo dell'iperbole, ossia il valore della Vmax
coef(NLR2)
Km = coef(NLR2)[1] # Attribuisco il valore numerico alla Km
Vm = coef(NLR2)[2] # Attribuisco il valore numerico alla Vm

#Disegna la curva usando i parametri stimati
curve(Vm * (1-exp(-1*Km*x)), from = 0, to = 20, add = TRUE, col = "black") 





