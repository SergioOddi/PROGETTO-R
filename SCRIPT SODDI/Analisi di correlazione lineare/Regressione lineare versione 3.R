###### ANALISI regressione lineare
###### VERSIONE 3.0 --- 13 marzo 2023

###### Librerie necessarie
# per il grafico
library(ggplot2)
library(ggprism)
library(ggpubr)
library(ggnewscale)
library(matrixStats)
library(drc)
# solo per manipolare il data.frame
library(dplyr)
library(tidyr)
library(rstatix)

# costruisce il data.frame, trasforma in log10 la concentrazione dell'agonista
# converte il data.frame in formato "long", quindi rimuove eventuali righe con NA


dati <- curva_nitrito


### Rinomino per comodità le colonne…
names(dati) <- c("Treatment", "x", "1", "2", "3")

###Seleziono le colonne su cui voglio calcolare media e ds su ogni riga
selezionate <- c("1", "2", "3")
### Se nel dataset ci sono campi vuoti (NA) usare il comando na.rm=TRUE
dati <- dati %>% mutate(mean = rowMeans(.[selezionate], na.rm=TRUE), stdev = rowSds(as.matrix(.[selezionate]), na.rm=TRUE), sem = rowSds(as.matrix(.[selezionate])/sqrt(3), na.rm=TRUE))  #### NB: la deviazione standard non è calcolabile a partire da un unico valore...


# Define model (note x and ec50 are swapped around because ec50 is already -ve)
retta <- y ~ m*x + q
#dose_resp <- y ~ min + ((max - min) / (1 + 10^(hill_coefficient * (ec50 - x))))


##### Comando per specificare nella funzione "stat_regline_equation" il numero di cifre significative da riportare nell'equazione 
trace(ggpubr:::.stat_lm, edit = TRUE) 

####NOTE per la compilazione:
### rr <- summary(res.lm)$r.squared %>% signif(5) ### linea che serve per stabilire le cifre di R2
### adj.rr <- summary(res.lm)$adj.r.squared %>% signif(2)
### AIC <- stats::AIC(res.lm) %>% signif(2)
### BIC <- stats::BIC(res.lm) %>% signif(2)
### eq.char <- as.character(signif(polynom::as.polynomial(coefs), c(2,5) ))  ### linea per stabilire le cifre dei coefficienti della retta: c(n1:intercetta, n2: coefficiente angolare)

curva <- lm(formula = mean ~ x, data=dati)

do=0      #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=12   

p <- ggplot(data=dati, aes(x = x, y = mean, group = Treatment, colour= Treatment))+
  #  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=0.05, position=position_dodge(do))+ #### DS
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), width=2, 
                position=position_dodge(0)
                #               position = "identity" (posizione identica)
  )+  #### Errore standard (SEM)
  
#geom_smooth(aes(colour = Treatment), method = "nls", formula = retta, se = FALSE,linewidth = 0.5 ,method.args = list(start = list(m = 1, q = 1), algorithm = "default"))+

  geom_smooth(aes(colour = Treatment), method = "lm", se = FALSE, linewidth = 0.5)+  #### Fitting lineare; spessore linea e colore
  #geom_point(position=position_dodge(do), size=2)+ #### dimensione e colore dei punti e delle barre di errore
  stat_regline_equation(label.y = c(0.75, 0.7, 0.65), #### label.y definisce la posizione dell'equazione lungo l'asse y delle rette interpolate: (y1, y2, y3)
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+  
  
  
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
  labs(y = "Absorbance") +              #### Titolo asse y
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
    axis.ticks.x = element_line(size=1, color="black"),    # Cambia solo i tick dell'asse x
    axis.ticks.y = element_line(size=1, color="black"),    # Cambia solo i tick dell'asse y
    #    axis.ticks.length = unit(3, "pt") # Cambia la lunghezza dei segni di graduazione
    legend.title =  element_blank(),
    legend.text = element_text(size=14, angle = 0, hjust = 1, face="bold", colour="#00167B"),
    legend.position = c(0.01, 1),             #### coordinate legenda (x e y)
    legend.justification = c(0.05, 1)) + 
  labs(x = "Concentration (μM)")                   #### Titolo asse x


p



?stat_regline_equation