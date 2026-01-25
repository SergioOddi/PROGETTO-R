###### ANALISI dati FRAP e disegno della curva ottenuta...
###### VERSIONE 1.0 -- 27 Novembre 2021
library(Bessel)
library("dplyr")

##### Carico data set ##########################################################
dati <- FRAP12_exp7

##### Attenzione il modello non accetta di partire con x = 0... togliere la riga con il valore nullo delle ascisse!
dati <- dati[-c(1:6), ]

#Se serve rinominare colonne
dati <- dati %>% rename("time" = "Time", "F0" ="F") #cambia il nome alla colonna da #Time a time

##### Modello da fittare ##########################################################
curva <- nls(F0 ~ A*exp((-2*tau)/time)*(BesselI((2*tau)/time, 0)+BesselI((2*tau)/time, 1)) + B, data=dati,
             start = list(A = 1, tau = 1, B = 0),
             algorithm = "port") ##### F04 è il nome del vettore con la variabile indipendente (fluorescenza), e time è il nome del vettore con la variabile indipendente (tempo in secondi) 
#F04 sono i valori della variabile risposta, time i valori della variabile indipendente del data set; A, tau e #B sono, risp., il primo, #il secondo, e il terzo coefficiente del modello scelto 

summary(curva)

##### Plotta i dati grezzi
plot(F0 ~ time, data = dati, ylim =c(0.2, 1), #### intervallo delle ordinate
     ylab = "Normalized Fluorescence",
     xlab = Time  ~ (s))

##### For each value of x, I can get the value of y estimated by the model, and add it to the current plot!
myPredict <- predict( curva ) 
ix <- sort(dati$time,index.return=T)$ix
lines(dati$time[ix], myPredict[ix], col=2, lwd=2)  
abline(h = coef(curva)[1]+coef(curva)[3], lty = 2)#traccia una linea segmentata orizzontale sul valore del ottenuto sommando i coef n.1 [1] 
#e n.3 [3], cioè A e B del modello

coef(curva)

# I add the features of the model to the plot
coeff <- round(coef(curva), 2)
text(80, 0.6 , paste("Coefficients: ", "A = ",coeff[1] , "; tau = " , coeff[2] , "; B = " , coeff[3]))




###### In alternativa definisco la funzione
FRAP <- function(A, tau, time, B) (A*exp((-2*tau)/time)*(BesselI((2*tau)/time, 0)+BesselI((2*tau)/time, 1)) + B)    #### time è il nome del vettore con la variabile indipendente, A e tau sono le costanti da determinare

#### Faccio fittare i dati sperimentali con il mio modello (Fading)
curva2 <- nls(F0 ~ FRAP(A, tau, time, B), data= dati   ####FCell3 è il vettore con i valori della variabile dipendente
            # , start=list(A=25,tau=5)
)

summary(curva2)

