###### Studio di regressione lineare sui livelli eCB nel plasma di soggetti AD
###### 9 giugno 2021



library("tidyverse")
library("dplyr")
require(plotly)

dati <-  PLASMA_ECS_level_Foglio1
#### Se occorre rinominare le colonne
dati  <-  dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("MMSE" = "MMSE (on 30)")

#### Se occorre escludere qualche gruppo
dati <- subset(dati, GROUP != "ctrl")


dati <- subset(dati, GROUP == "AD MODERATE")

dati  <-  dati %>% mutate(AG_log = log10(AG))
dati  <-  dati %>% mutate(AEA_log = log10(AEA))
dati  <-  dati %>% mutate(PEA_log = log10(PEA))
dati  <-  dati %>% mutate(OEA_log = log10(OEA))

head(dati)

p <- plot_ly(dati, x = ~MMSE, y = ~AEA_log, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter", mode ="markers", 
             color = as.factor(dati$GROUP)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "Scatter plot",
    scene = list(
      xaxis = list(title = "MMSE"),
      yaxis = list(title = "log[2-AG]")
    ))
p

p <- plot_ly(dati, x = ~MMSE, y = ~AEA_log, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter", mode ="markers", 
             color = as.factor(dati$GROUP)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "Scatter plot",
    scene = list(
      xaxis = list(title = "MMSE"),
      yaxis = list(title = "log[2-AG]")
    ))
p

#LIBRERIE da utilizzare
library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(ggpubr)
library(rstatix)




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

dati$MMSE=as.numeric(dati$MMSE)

q <- ggplot(dati, aes(x=MMSE,               #nome colonna Variabile indipendente
                      y=OEA_log))+          #nome colonna Variabile dipendente 
  geom_point(shape = 15, size = 3, color = "black") +   #forma, dimensione e colore dei punti
  stat_smooth(method = "lm", formula= y~x, color = "red", size =0.5) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
q



############ moderate
dati <-  PLASMA_ECS_level_Foglio1
#### Se occorre rinominare le colonne
dati  <-  dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("MMSE" = "MMSE (on 30)")
dati <- subset(dati, GROUP == "AD MODERATE")

dati  <-  dati %>% mutate(AG_log = log10(AG))
dati  <-  dati %>% mutate(AEA_log = log10(AEA))
dati  <-  dati %>% mutate(PEA_log = log10(PEA))
dati  <-  dati %>% mutate(OEA_log = log10(OEA))

q <- ggplot(dati, aes(x=MMSE,             #nome colonna Variabile indipendente
                      y=AG_log))+          #nome colonna Variabile dipendente 
  geom_point(shape = 15, size = 3, color = "black") +   #forma, dimensione e colore dei punti
  stat_smooth(method = "lm", formula= y~x, color = "red", size =0.5) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
q







library(ggplot2) 
library(plotly) 
library(car)






dati3 <- dati[, c(2,7,13:16)]
head(dati3)
#MATRICE di correlazione con dati raggruppati
library(GGally)

dati3 %>% ggpairs(.,  legend = 1,
                 
                 mapping = ggplot2::aes(colour=GROUP), #Group è la colonna con la variabile categorica con due fattori GF e PF
                 lower = list(continuous = wrap("smooth", alpha = 0.3, size=1.5), #qui size determina la dimensione del punto
                              discrete = "blank", combo="blank"), 
                 diag = list(discrete="barDiag", 
                             continuous = wrap("densityDiag", alpha=0.5 )), 
                 upper = list(combo = wrap("box_no_facet", alpha=0.5),
                              continuous = wrap("cor", size=3, alignPercent=0.8))) + #qui size determina la dimensione del testo
  theme(panel.grid.major = element_blank())    # remove gridlines 



dati <-  data_cani
#### Se occorre rinominare le colonne
dati  <-  dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("MMSE" = "MMSE (on 30)")

#### Se occorre escludere qualche gruppo
dati <- subset(dati, Group != "	healthy")


dati <- subset(dati, GROUP == "AD MODERATE")

dati  <-  dati %>% mutate(AG_log = log10(AG))
dati  <-  dati %>% mutate(AEA_log = log10(AEA))
dati  <-  dati %>% mutate(PEA_log = log10(PEA))
dati  <-  dati %>% mutate(OEA_log = log10(OEA))

head(dati)

p <- plot_ly(dati, x = ~MMSE, y = ~AEA_log, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter", mode ="markers", 
             color = as.factor(dati$GROUP)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "Scatter plot",
    scene = list(
      xaxis = list(title = "MMSE"),
      yaxis = list(title = "log[2-AG]")
    ))
p

p <- plot_ly(dati, x = ~MMSE, y = ~AEA_log, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter", mode ="markers", 
             color = as.factor(dati$GROUP)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "Scatter plot",
    scene = list(
      xaxis = list(title = "MMSE"),
      yaxis = list(title = "log[2-AG]")
    ))
p

#LIBRERIE da utilizzare
library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(ggpubr)
library(rstatix)




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

dati$MMSE=as.numeric(dati$MMSE)

q <- ggplot(dati, aes(x=MMSE,               #nome colonna Variabile indipendente
                      y=OEA_log))+          #nome colonna Variabile dipendente 
  geom_point(shape = 15, size = 3, color = "black") +   #forma, dimensione e colore dei punti
  stat_smooth(method = "lm", formula= y~x, color = "red", size =0.5) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
q



j############ moderate
dati <-  PLASMA_ECS_level_Foglio1
#### Se occorre rinominare le colonne
dati  <-  dati %>% rename("AG" = "2-AG")
dati <- dati %>% rename("MMSE" = "MMSE (on 30)")
dati <- subset(dati, GROUP == "AD MODERATE")

dati  <-  dati %>% mutate(AG_log = log10(AG))
dati  <-  dati %>% mutate(AEA_log = log10(AEA))
dati  <-  dati %>% mutate(PEA_log = log10(PEA))
dati  <-  dati %>% mutate(OEA_log = log10(OEA))

q <- ggplot(dati, aes(x=MMSE,             #nome colonna Variabile indipendente
                      y=AG_log))+          #nome colonna Variabile dipendente 
  geom_point(shape = 15, size = 3, color = "black") +   #forma, dimensione e colore dei punti
  stat_smooth(method = "lm", formula= y~x, color = "red", size =0.5) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
q


dati2 <- dati[-c(11,14), ]

Tg <- c(10.5, 11.4, 6.6, 0.9, 16.1, 24.9, 7.0, 36.1, 9.8, 9.1, 9.0, 8.0)
Wt <- c(15.7, 14.8, 10.6, 6.5, 16.0, 14.3, 17.6, 7.6, 4.1, 27.7, 2.3)

library(ggplot2) 
library(plotly) 
library(car)

# Ci dà un’idea se i dati sono distribuiti in modo normale, i punti fuori dalla zona di confidenza sono probabili outlier
qqPlot(Tg)
qqPlot(Wt)

with(dati2, qqPlot(PEA, dist="norm", id=list(method="y", n=2, labels=rownames(dati2))))


Matrice di correlazione plot





dati3 <- dati[, c(2,10:13,14)]
head(dati3)
#MATRICE di correlazione con dati raggruppati
library(GGally)

dati3 %>% ggpairs(.,  legend = 1,
                  
                  mapping = ggplot2::aes(colour=GROUP), #Group è la colonna con la variabile categorica con due fattori GF e PF
                  lower = list(continuous = wrap("smooth", alpha = 0.3, size=1.5), #qui size determina la dimensione del punto
                               discrete = "blank", combo="blank"), 
                  diag = list(discrete="barDiag", 
                              continuous = wrap("densityDiag", alpha=0.5 )), 
                  upper = list(combo = wrap("box_no_facet", alpha=0.5),
                               continuous = wrap("cor", size=3, alignPercent=0.8))) + #qui size determina la dimensione del testo
  theme(panel.grid.major = element_blank())    # remove gridlines 



