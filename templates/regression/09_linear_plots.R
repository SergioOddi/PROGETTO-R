######### Timepoints con media ed errori 
######### 05 giugno 2021

library(ggplot2)
library(tidyverse)


#### Carico dataset standard con una colonna in cui è riportata la d.s. (se) dei valori in Value 
dati <- Esempio_di_curve_Foglio1

# A tibble: 6 x 4
# ID   Age Value    se
#<dbl> <dbl> <dbl> <dbl>
#   1     1     1   0.1
#   1     2     2   0.2
#   1     3     3   0.3
#   1     4     4   0.4
#   2     1     2   0.1
#   2     2     4   0.2

#### Se occorre selezionare qualche gruppo
#dati <- subset(dati, ID == "1")


#### Se occorre rinominare le colonne
dati = dati %>% rename("time" = "Time")

## Cambiare in fattori i vettori categorici ID e Age
dati$ID <- as.factor(dati$ID)
dati$Age <- as.factor(dati$Age)

do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12  #### stabilire la dimensione dei font sul grafico

ggplot(data = dati, aes(x = Age, 
                        y = Value, 
                        group= ID, 
                        colour=ID, ### Se lo escludo fa i punti neri
                        shape = ID))+
  geom_line(position=position_dodge(do), size =0.5, linetype = 1)+
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=0.3, position=position_dodge(do))+
  geom_point(position=position_dodge(do), size=4)+
  labs(title="",                          #se serve il titolo
       x = "Age (month)",                    #nome delle ascisse
       y = "AEA (nM)") +                  #nome delle ordinate
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
  

##### Per un grafico in bianco e nero, i punti dentro bianchi

do=0.0    #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12  #### stabilire la dimensione dei font sul grafico

ggplot(data = dati, aes(x = Age, 
                        y = Value, 
                        group= ID, 
                        #colour= ID, ### Se lo escludo fa i punti neri
                        shape = ID))+
  geom_line(position=position_dodge(do), size =0.5, linetype = 1)+
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=0.3, position=position_dodge(do))+
  geom_point(position=position_dodge(do), size=4)+
  geom_point(position=position_dodge(do), size=3, color = "white")+  ### per "colorare" in bianco l'interno dei punti
  labs(title="",                          #se serve il titolo
       x = "Age (month)",                    #nome delle ascisse
       y = "AEA (nM)") +                  #nome delle ordinate
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







