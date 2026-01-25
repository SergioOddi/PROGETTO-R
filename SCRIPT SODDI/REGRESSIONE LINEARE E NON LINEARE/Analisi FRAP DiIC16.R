###### ANALISI dati FRAP e disegno della curva ottenuta...
###### VERSIONE 1.0 --- 3 Dicembre 2021

###### Librerie necessarie (verificare quali)

library(Bessel)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)

##### Carico data set ##########################################################
dati <- DiIC16

##### Attenzione il modello non accetta di partire con time = 0... (togliere la riga con il valore nullo delle ascisse!)
# dati <- dati[-c(1:6), ]

#Se serve rinominare colonne
# dati <- dati %>% rename("time" = "Time", "F0" ="F") #cambia il nome alla colonna da #Time a time


##### Modello da fittare ###### F03 variabile dip. ### time: variabile ind. ####################################
curva <- nls(DiF5 ~ A*exp((-2*tau)/time)*(BesselI((2*tau)/time, 0)+BesselI((2*tau)/time, 1)) + B, data=dati, ##### A, tau e #B sono, risp., il primo, #il secondo, e il terzo coefficiente del modello scelto 
             start = list(A = 1, tau = 1, B = 0),
             algorithm = "port")
summary(curva)

##### Plotta i dati grezzi: cambiare il nome della variabile dip... FO3... con quello della colonna di interesse
plot(DiF5 ~ time, data = dati, ylim =c(0.2, 1), #### intervallo delle ordinate
     ylab = "Normalized Fluorescence",
     xlab = Time  ~ (s))

##### For each value of x, I can get the value of y estimated by the model, and add it to the current plot!
myPredict <- predict( curva ) 
ix <- sort(dati$time,index.return=T)$ix
lines(dati$time[ix], myPredict[ix], col=2, lwd=2)  
abline(h = coef(curva)[1]+coef(curva)[3], lty = 2)#traccia una linea segmentata orizzontale sul valore del ottenuto sommando i coef n.1 [1] 
#e n.3 [3], cioè A e B del modello

# I add the features of the model to the plot
coeff <- round(coef(curva), 2)
text(80, 0.6 , paste("Coefficients: ", "A = ",coeff[1] , "; tau = " , coeff[2] , "; B = " , coeff[3]))
coef(curva)


### Rinomino per comodità le variabili...
names(dati) <- c("time", "1", "2", "3", "4", "5", "6", "7", "8", "9")

###Seleziono le variabili che mi interessano
selezionate <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
selezionate 

#### Per mediare e fare ds delle n. colonne e farne la FRAP
dati <- dati %>% mutate(mean = rowMeans(.[selezionate]), stdev = rowSds(as.matrix(.[selezionate])))

##### Modello da fittare ###### F03 variabile dip. ### time: variabile ind. ####################################
curva <- nls(mean ~ A*exp((-2*tau)/time)*(BesselI((2*tau)/time, 0)+BesselI((2*tau)/time, 1)) + B, data=dati, ##### A, tau e #B sono, risp., il primo, #il secondo, e il terzo coefficiente del modello scelto 
             start = list(A = 1, tau = 1, B = 0),
             algorithm = "port")
summary(curva)



# I add the features of the model to the plot
coeff <- round(coef(curva), 2)
text(80, 0.6 , paste("Coefficients: ", "A = ",coeff[1] , "; tau = " , coeff[2] , "; B = " , coeff[3]))
coef(curva)


###### Per disegnare la curva con i punti i loro errori e la curva interpolante!!! 
do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12  #### stabilire la dimensione dei font sul grafico

ggplot(data = dati, aes(x = time, 
                        y = mean 
                        #, group= ID, 
                        #colour=ID, 
                        #shape = ID
))+
  geom_line(position=position_dodge(do), size =0.4, linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.3, position=position_dodge(do))+
  stat_smooth(method = 'nls', formula = 'y ~ A*exp((-2*tau)/x)*(BesselI((2*tau)/x, 0)+BesselI((2*tau)/x, 1)) + B',                     #Fit nls model
              method.args = list(start=c(A = 1, tau = 1, B = 0), algorithm = "port"), se=FALSE, size =0.5, col = "red") +
  geom_point(position=position_dodge(do), size=1)+
  labs(title="",                          #se serve il titolo
       x = "Time (s)",                    #nome delle ascisse
       y = "Normalized Fluorescence") +   #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda "none" 
  )










