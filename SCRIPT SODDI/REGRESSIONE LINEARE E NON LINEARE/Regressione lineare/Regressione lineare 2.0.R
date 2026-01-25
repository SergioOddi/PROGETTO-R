######### REGRESSIONE LINEARE
######### Versione 1.2
######### 22 gennaio 2022

library(ggplot2)
library(dplyr)
library(matrixStats)
library(ggpubr)
library(rstatix)

#### Carico dataset standard con una colonna in cui è riportata la d.s. (se) dei valori in Value 
dati <- curva_nitrito

### Rinomino per comodità le variabili...
names(dati) <- c("ID", "concentration", "1", "2", "3")

###Seleziono le variabili che mi interessano
selezionate <- c("1", "2", "3")

#### Per mediare e fare ds delle n. colonne
dati <- dati %>% mutate(mean = rowMeans(.[selezionate]), stdev = rowSds(as.matrix(.[selezionate])))

###### il dataset "dati" ora contiene due nuove colonne: mean (la media delle colonne selezionate) e  ‘stdev’ con la deviazione standard delle colonne


#### Se occorre selezionare qualche gruppo
#dati <- subset(dati, ID == "1")

#### Se occorre escludere qualche gruppo
#dati <- subset(dati, ID != "1")

#### Se occorre rinominare le colonne
#dati = dati %>% rename("time" = "Time")

##### Modello da fittare ###### mean variabile dip. ### concentration: variabile ind. ####################################
# curva <- lm(formula = mean ~ concentration, data=dati)
# summary(curva)

###### Per disegnare la curva con i punti i loro errori e la curva interpolante!!! 
do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12  #### stabilire la dimensione dei font sul grafico



######### Funzione Regressione lineare
stat_regline_equation(
  mapping = NULL,
  data = NULL,
  formula = y ~ x,
  label.x.npc = "left",
  label.y.npc = "top",
  label.x = NULL,
  label.y = NULL,
  output.type = "expression",
  geom = "text",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)


##### Comando per specificare il numero di cifre significative da riportare nell'equazione
trace(ggpubr:::.stat_lm, edit = TRUE) 

####NOTE per la compilazione
### rr <- summary(res.lm)$r.squared %>% signif(5) ### linea per stabilire le cifre di R2
### adj.rr <- summary(res.lm)$adj.r.squared %>% signif(2)
### AIC <- stats::AIC(res.lm) %>% signif(2)
### BIC <- stats::BIC(res.lm) %>% signif(2)
### eq.char <- as.character(signif(polynom::as.polynomial(coefs), c(2,5) ))  ### linea per stabilire le cifre dei coefficienti della retta: c(n1:intercetta,n2: coefficiente angolare)


ggplot(data = dati, aes(x = concentration, 
                        y = mean 
                        , group= ID #### Se ci sono più gruppi
                        , colour= ID 
                        , shape = ID
))+
  geom_line(position=position_dodge(do), linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.3, position=position_dodge(do))+
  geom_smooth(method = "lm", se = FALSE, size =0.4, colour ="black")+  ####Linear fitting; spessore linea e colore
  #stat_smooth(method = 'lm', formula = 'y ~ x'                     #Fit nls model
  #            ), se=TRUE, size =0.5) +
  geom_point(position=position_dodge(do), size=2)+ #### dimensione e colore dei punti e delle barre di errore
stat_regline_equation(label.y = c(1, 0.9, 0.8), aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+  #### label.y definisce la posizione dell'equazione lungo l'asse y (y1, y2, y3)

   labs(title="",                          #se serve il titolo
       x = "Concentration (uM)",                    #nome delle ascisse
       y = "Absorbance") +                          #nome delle ordinate

  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 0.5, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda "none" ; "right"
  )






