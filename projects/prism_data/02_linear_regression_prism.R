###### ANALISI regressione lineare
###### VERSIONE 3.0 --- 13 marzo 2023

###### Librerie necessarie
# for the graph
library(ggplot2)
library(ggprism)

library(ggnewscale)
library(matrixStats)
library(drc)
# just for manipulating the data.frame
library(dplyr)
library(tidyr)

# construct the data.frame, log10 transform the agonist concentration
# convert the data.frame to long format, then remove any rows with NA


dati <- curva_nitrito
#dati <- dati[, c(1, 3:7)]



### Rinomino per comodità le colonne…
names(dati) <- c("Treatment", "Concentration", "x", "1", "2", "3")

###Seleziono le colonne su cui voglio calcolare media e ds su ogni riga
selezionate <- c("1", "2", "3")
### Se nel dataset ci sono campi vuoti (NA) usare il comando na.rm=TRUE
dati <- dati %>% mutate(mean = rowMeans(.[selezionate], na.rm=TRUE), stdev = rowSds(as.matrix(.[selezionate]), na.rm=TRUE), sem = rowSds(as.matrix(.[selezionate])/sqrt(3), na.rm=TRUE))  #### NB: la deviazione standard non è calcolabile a partire da un unico valore...


# Define model (note x and ec50 are swapped around because ec50 is already -ve)
retta <- y ~ m*x + q
#dose_resp <- y ~ min + ((max - min) / (1 + 10^(hill_coefficient * (ec50 - x))))



do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12   

p <- ggplot(data=dati, aes(x = x, y = mean))+
  #  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.05, position=position_dodge(do))+ #### DS
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=0.8, 
                position=position_dodge(0.8)
                #               position = "identity"
  )+  #### SEM
  geom_smooth(aes(colour = Treatment), method = "nls", formula = retta, se = FALSE,linewidth = 0.5 
              ,method.args = list(start = list(m = 1, q = 1)
                                , algorithm = "default")
              )+
  #geom_smooth(aes(colour = Treatment), method = drm, method.args = list(fct = L.4(names = c("Hillslope", "Bottom", "Top", "EC50"))), se = F, linewidth = 0.5)+
  
  scale_colour_manual(labels = c("CTRL", "Treated", "Treated2")
                    ,values = c("#9FA3FE", "#026FEE", "#00167B") #### altro colore: "#87FFFF"; "#9FA3FE"; 
                              ) +
  
  ggnewscale::new_scale_colour() +
  
  geom_point(aes(colour = Treatment, shape = Treatment), size = 4, position=position_dodge(do))+ 
  
  scale_colour_prism(palette = "winter_bright", 
                     labels = c("CTRL", "Treated", "Treated2")) +
  
  scale_shape_prism(labels = c("CTRL", "Treated", "Treated2")) + 
  
  theme_prism(palette = "winter_bright", base_size = 16, base_family = "Arial") + 
  
  scale_y_continuous(limits = c(0, 1) 
                     #      ,breaks = seq(0, 1, 0.25)
                     #      ,guide = "prism_offset"
  ) +
  labs(y = "Normalized Fluorescence") +        #### Titolo asse y
  scale_x_continuous(limits = c(0, 50) 
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
  labs(x = "Concentration (μM)")          #### Titolo asse x


p











### Seleziono un trattamento alla volta
#### CBD
dati_CBD <- subset(dati, Treatment == "CBD")
curva_CBD <- drm(mean ~ Concentration, fct=LL.4(names = c("Hillslope", "Bottom", "Top", "EC50")), data = dati_CBD)
#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with
plot(curva_CBD, type="all")
summary(curva_CBD)
confint(curva_CBD, level = 0.90)

#We can convert the inferred values to an IC50.
coefs <- setNames(
  curva_CBD$coefficients,
  c("hill", "min_value", "max_value", "ec_50")
)
coefs 

ic_50 <- with(
  as.list(coefs),
  exp(
    log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
  )
)
ic_50 



##### GLA
dati_GLA <- subset(dati, Treatment == "GLA")
curva_GLA <- drm(mean ~ Concentration, fct=LL.4(names = c("Hillslope", "Bottom", "Top", "EC50")), data = dati_GLA)
#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with
plot(curva_GLA, type="all")
summary(curva_GLA)

#We can convert the inferred values to an IC50.
coefs <- setNames(
  curva_GLA$coefficients,
  c("hill", "min_value", "max_value", "ec_50")
)
coefs 

ic_50 <- with(
  as.list(coefs),
  exp(
    log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
  )
)
ic_50 


##COMB
dati_COMB <- subset(dati, Treatment == "COMB")
curva_COMB <- drm(mean ~ Concentration, fct=LL.4(names = c("Hillslope", "Bottom", "Top", "EC50")), data = dati_COMB)
#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with
plot(curva_COMB, type="all")
summary(curva_COMB)

#We can convert the inferred values to an IC50.
coefs <- setNames(
  curva_COMB$coefficients,
  c("hill", "min_value", "max_value", "ec_50")
)
coefs 

ic_50 <- with(
  as.list(coefs),
  exp(
    log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
  )
)
ic_50 


