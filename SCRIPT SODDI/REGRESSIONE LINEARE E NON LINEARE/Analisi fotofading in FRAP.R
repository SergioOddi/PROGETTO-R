###### ANALISI Photofading rate exp di FRAP e disegno della curva ottenuta...
###### VERSIONE 1.0 -- 29 Novembre 2021


##### Carico data set ##########################################################
datiF <- ANALISI_FRAP_exp07_xlsx_x_calcolo_fading

###### Definisco la funzione
Fading <- function(time, A, tau) (A*exp(-time/tau))    #### time è il nome del vettore con la variabile indipendente, A e tau sono le costanti da determinare

#### Faccio fittare i dati sperimentali con il mio modello (Fading)
curva <- nls(Fcell3 ~ Fading(time,A,tau), data= datiF   ####FCell3 è il vettore con i valori della variabile dipendente
   , start=list(A=25,tau=5)
    )

summary(curva)

plot(Fcell3 ~ time, data = datiF, ylim =c(15, 27), #### intervallo delle ordinate
     ylab = "Fluorescence",
     xlab = Time  ~ (s))

myPredict <- predict( curva ) 
ix <- sort(datiF$time,index.return=T)$ix
lines(datiF$time[ix], myPredict[ix], col=2, lwd=2) 
