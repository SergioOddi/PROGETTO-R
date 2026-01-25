#### FRAP
###### ANALISI dati FRAP e disegno della curva ottenuta...
###### VERSIONE 1.0 --- 3 Dicembre 2021
### VERSIONE 2.0 --- 4 Marzo 2023
###### Librerie necessarie (verificare quali)

library(Bessel)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)

##### Carico data set ##########################################################
dati <- FRAP

###Seleziono le colonne su cui voglio calcolare media e ds su ogni riga
selezionate <- c("F1", "F2", "F3")
### Se nel dataset ci sono campi vuoti (NA) usare il comando na.rm=TRUE
dati <- dati %>% mutate(mean = rowMeans(.[selezionate], na.rm=TRUE), stdev = rowSds(as.matrix(.[selezionate]), na.rm=TRUE), sem = rowSds(as.matrix(.[selezionate])/sqrt(3), na.rm=TRUE))  #### NB: la deviazione standard non è calcolabile a partire da un unico valore...


###### Per disegnare la curva con i punti i loro errori e la curva interpolante!!! 
do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=14  #### stabilire la dimensione dei font sul grafico


###### il dataset dati ora contiene due nuove colonne: mean (la media delle colonne selezionate) e  ‘stdev’ con la deviazione standard delle colonne

ggplot(data = dati, aes(x = time, y = mean))+
#  geom_line(aes(colour=ID), position=position_dodge(do), size =0.4, linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=2, size=0.4, position=position_dodge(0.8)) +
#  stat_smooth(aes(colour=ID), method = 'nls', formula = 'y ~ A*exp((-2*tau)/x)*(BesselI((2*tau)/x, 0)+BesselI((2*tau)/x, 1)) + B',                     #Fit nls model
#              method.args = list(start=c(A = 1, tau = 1, B = 0), algorithm = "port"), se=FALSE, size =0.5) +
  geom_smooth(aes(colour = ID), method = 'nls', formula = 'y ~ A*exp((-2*tau)/x)*(BesselI((2*tau)/x, 0)+BesselI((2*tau)/x, 1)) + B',                     #Fit nls model
              method.args = list(start=c(A = 1, tau = 1, B = 0), algorithm = "port"), se=FALSE, size =0.5) +
    scale_colour_manual(labels = c("Tg", "WT"), ##### ATTENZIONE all'ordine di assegnazione... non è chiaro il risultato  
                      values = c("#00167B", "#9FA3FE")#### altri colori: "#026FEE" "#87FFFF"
  ) + 
  ggnewscale::new_scale_colour() +
  
  geom_point(aes(colour = ID, shape = ID), size = 4, position=position_dodge(do))+ 
  
  scale_colour_prism(palette = "winter_bright", 
                     labels = c("Tg", "WT")) +
  
  scale_shape_prism(labels = c("Tg", "WT")) + 
  
  theme_prism(palette = "winter_bright", base_size = 16, base_family = "Arial") + 
  
  scale_y_continuous(limits = c(0, 1.05) 
               #      ,breaks = seq(0, 1, 0.25)
               #      ,guide = "prism_offset"
              ) +
  labs(y = "Normalized Fluorescence") +        #### Titolo asse y
  scale_x_continuous(limits = c(0, 102) 
#                    ,breaks = seq(0, 120, 25),
#                    ,guide = "prism_offset"
#minor_breaks = log10(rep(1:9, 7)*(10^rep(-10:-4, each = 9))),
#    labels = function(lab) {
#     do.call(
#        expression,
#       lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
#    )
# }
  ) + 
  theme(
    # axis.title.y = element_blank(),  ### Per togliere il titolo all'asse y
    axis.line.x = element_line(colour = "black", size = 1),  # Colore Linea dell'asse x
    axis.line.y = element_line(colour = "black", size = 1),  # Colore Linea dell'asse y
    axis.text.x = element_text(size=sizeT, angle = 0, hjust = 0.5, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
    axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
    axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
    axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
    axis.ticks.x = element_line(size=1, color="black"),    # Change x axis ticks only
    axis.ticks.y = element_line(size=1, color="black"),    # Change y axis ticks only
#    axis.ticks.length = unit(3, "pt") # Change the length of tick marks
    legend.title =  element_blank(),
    legend.text = element_text(size=14, angle = 0, hjust = 1, face="bold", colour="#00167B"),
    legend.position = c(0.01, 1),   #### coordinate legenda
    legend.justification = c(0.05, 1)) + 
    labs(x = "Time (s)")          #### Titolo asse x

  
##### OLD VERSION
ggplot(data = dati, aes(x = time, y = mean))+
  
  geom_line(aes(colour=ID), position=position_dodge(do), size =0.4, linetype = 0.1)+
  
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev),  width=2, size=0.4, position=position_dodge(do))+
  
  stat_smooth(aes(colour=ID), method = 'nls', formula = 'y ~ A*exp((-2*tau)/x)*(BesselI((2*tau)/x, 0)+BesselI((2*tau)/x, 1)) + B', #Fit nls model
              method.args = list(start=c(A = 1, tau = 1, B = 0), algorithm = "port"), se=FALSE, size =0.5) +
  
  geom_point(aes(colour = ID, shape = ID), size = 3, position=position_dodge(do))+
  
  labs(title="",                          #se serve il titolo
       x = "Time (s)",                    #nome delle ascisse
       y = "Normalized Fluorescence") +   #nome delle ordinate
  
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 0.5, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_blank(),
        legend.text = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = c(0.05, 0.95) ### coordinate 
        #legend.position = "none" ##### Se non si vuole la legenda "none" 
  )







