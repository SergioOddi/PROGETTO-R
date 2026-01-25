###### ANALISI dati FRAP e disegno della curva ottenuta
###### VERSIONE 2.0 --- 13 Ottobre 2024

###### Librerie necessarie

library(Bessel)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)

citation(package ="ggplot2")

##### Carico data set ##########################################################
dati <- FRAP_EXP04

##### Attenzione il modello non accetta di partire con time = 0... (togliere la riga con il valore nullo delle ascisse!)
# dati <- dati[-c(1:6), ]

#Se serve rinominare colonne
# dati <- dati %>% rename("time" = "Time", "F0" ="F") #cambia il nome alla colonna da #Time a time


### Rinomino per comodità le variabili...
names(dati) <- c("time", "1", "2", "3", "4", "5", "6")

###Seleziono le variabili che mi interessano
selezionate <- c("1", "2", "3", "4", "5", "6")

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
sizeT=16  #### stabilire la dimensione dei font sul grafico


##### OLD VERSION
ggplot(data = dati, aes(x = time, 
                        y = mean 
                        #, group= ID, 
                        # colour=ID, 
                        # shape = ID
))+
  geom_line(position=position_dodge(do), size =0.4, linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev),  width=2, size=0.4, position=position_dodge(do))+
  stat_smooth(method = 'nls', formula = 'y ~ A*exp((-2*tau)/x)*(BesselI((2*tau)/x, 0)+BesselI((2*tau)/x, 1)) + B',                     #Fit nls model
              method.args = list(start=c(A = 1, tau = 1, B = 0), algorithm = "port"), se=FALSE, size =0.5, col = "blue") +
  geom_point(size = 2, position=position_dodge(do))+
  labs(title="",                          #se serve il titolo
       x = "Time (s)",                    #nome delle ascisse
       y = "Normalized Fluorescence") +   #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Helvetica"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=20, angle = 0, hjust = 0.5, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=20, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=20, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=20, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda "none" 
  )













