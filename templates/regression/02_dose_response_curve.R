###### ANALISI dati Curva logistica
###### VERSIONE 1.0 --- 2 marzo 2023

###### Librerie necessarie (verificare quali)


library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)
library(nlme)
library(tidyverse)
library(drc)

##### Carico data set ##########################################################
dati <- Daniel
names(Daniel) <- c("Treatment", "C1", "C2", "x", "Vit")


#dati <- subset(Daniel, Treatment != "GLA")
dati <- subset(Daniel, Treatment == "COMB")
dati <- subset(Daniel, Treatment == "CBD")

# Define model (note x and ec50 are swapped around because ec50 is already -ve)
dose_resp <- y ~ min + ((max - min) / (1 + 10^(hill_coefficient * (ec50 - x))))


curva <- nls(Vit ~ min + ((max - min) / (1 + 10^(hill_coefficient * (ec50 - x)))), data = dati,
             start = list(min = 0, max = 100, hill_coefficient = -5, ec50 = -5),
             algorithm = "port", trace = TRUE
)

summary(curva)



curva<- drm(Vit~x, data=dati, fct=LL.4(names = c("Hillslope", "Bottom", "Top", "EC50")))
#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with
plot(curva, type="all")


?nls


p <- ggplot(data=dati, aes(x = x, y = Vit))+
  #geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=0.05, position=position_dodge(do))+  
 # geom_smooth(
  #  aes(colour = Treatment),
  #  method = "nls", 
  #  formula = dose_resp, 
  # se = FALSE,
  #  linewidth = 0.5,
   # method.args = list(start = list(min = 20, max = 100, ec50 = -5, hill_coefficient = -5), algorithm = "default") ##### 
  # )
geom_smooth(aes(colour = Treatment), method = drm, method.args = list(fct = L.4(names = c("Hillslope", "Bottom", "Top", "EC50"))), se = F)+
  scale_colour_manual(labels = c("CBD", "CBD/GLA", "COMB"),
                      values = c("#00167B", "#9FA3FE", "red")) + 
  ggnewscale::new_scale_colour() +
  geom_point(aes(colour = Treatment, shape = Treatment), size = 3) + 
  scale_colour_prism(palette = "winter_bright", 
                     labels = c("CBD", "CBD/GLA", "COMB")) + 
  scale_shape_prism(labels = c("CBD", "CBD/GLA", "COMB")) + 
  theme_prism(palette = "winter_bright", base_size = 16) + 
  scale_y_continuous(limits = c(0, 130), 
                     breaks = seq(0, 500, 10),
                     guide = "prism_offset") + 
  scale_x_continuous(
    limits = c(-7, -4), 
    breaks = -10:-3,
    guide = "prism_offset_minor",
    minor_breaks = log10(rep(1:9, 7)*(10^rep(-10:-4, each = 9))),
    labels = function(lab) {
      do.call(
        expression,
        lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
      )
    }
  ) + 
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0.05, 0.95)) + 
  labs(x = "[CBD], M")

p








geom_smooth(method = drm, col = "skyblue", method.args = list(fct = L.4(names = c("Hillslope", "Bottom", "Top", "EC50"))), se = F)




### Rinomino per comodità le variabili...
names(dati) <- c("Treatment", "x", "1", "2", "3")

###Seleziono le variabili che mi interessano
selezionate <- c("1", "2", "3")
selezionate 

#### Per mediare e fare ds delle n. colonne e farne la FRAP
dati <- dati %>% mutate(mean = rowMeans(.[selezionate]), stdev = rowSds(as.matrix(.[selezionate])))



###### Per disegnare la curva con i punti i loro errori e la curva interpolante!!! 
do=0.3      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12  #### stabilire la dimensione dei font sul grafico

library(scales)

###### il dataset dati ora contiene due nuove colonne: mean (la media delle colonne selezionate) e  ‘stdev’ con la deviazione standard delle colonne

p <- ggplot(data = dati, aes(x = x, 
                        y = mean 
                        #, group = ID 
                        #,colour = ID 
                        #, shape = ID
))+
  geom_line(position=position_dodge(do), size =0.4, linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.05, position=position_dodge(do))+
  stat_smooth(method = 'nls', formula = 'y ~ Bottom + ((x^Hillslope)*(Top-Bottom))/(x^Hillslope + EC50^Hillslope)',                     #Fit nls model
              method.args = list(start=c(Bottom = -5, Top = 100, Hillslope = 3, EC50 = 0.5), algorithm = "port"), se=FALSE, size =0.6, colour = "red") +
  geom_point(position=position_dodge(do), size=1)+
  labs(title="",                          #se serve il titolo
       x = "Concentration (logM)",                    #nome delle ascisse
       y = "Vitality") +   #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
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
p 

p <- p + scale_x_continuous(trans = log10_trans())
p <- p + scale_x_log10()
p 

#+ coord_trans(x="log10")
# scale_x_continuous(trans='log10')+ 
#  scale_x_log10()+ 




p <- ggplot(data = dati, aes(x = x, 
                             y = mean 
                             #, group = ID 
                             #,colour = ID 
                             #, shape = ID
))+
  geom_line(position=position_dodge(do), size =0.4, linetype = 0.1)+
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.05, position=position_dodge(do))+
  geom_smooth(method = 'nls', formula = 'y ~ Bottom + ((x^Hillslope)*(Top-Bottom))/(x^Hillslope + EC50^Hillslope)',                     #Fit nls model
              method.args = list(start=c(Bottom = -5, Top = 100, Hillslope = 3, EC50 = 0.5), algorithm = "port", aes(y=y, x =log10(x))), se=FALSE, size =0.6, colour = "red") +
  geom_point(position=position_dodge(do), size=1)+
  labs(title="",                          #se serve il titolo
       x = "Concentration (logM)",                    #nome delle ascisse
       y = "Vitality") +   #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda "none" 
  )  + scale_x_log10()
p 
















x <- log10(c(1e-8, 3e-8, 1e-7, 3e-7, 1e-6, 3e-6, 1e-5))
#x <- c(-8, -7.523, -7, -6.523, -6, -5.523, -5)

h <- 1
logk <- log10(3e-7)
ylo <- 30
yhi <- 330
#hyperbolic function
y=ylo+((yhi-ylo)/(1+10^((logk-x)*h))) + rnorm(length(x), 0, 45)

ggplot(
  data.frame(x, y), 
  aes(x, y))+
  geom_point(size=4, color="#f2a900")+
  geom_smooth(
    method=nls,
    formula = "y~ylo+((yhi-ylo)/(1+10^((logk-x)*h)))",
    method.args = list(
      start=c(yhi=150,
              ylo=50,
              logk=-7)
    ),
    se=F, #need this line for nls graphs and it is not obvious!!
    color="red"
  )+
  labs(title="Best fit for random Y values; Linearized X by Log10(X) transform")




