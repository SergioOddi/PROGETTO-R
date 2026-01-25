######### Regressione lineare
#21 dicembre 2020

#LIBRERIE da utilizzare
library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(ggpubr)
library(rstatix)


#Carico il dataset grezzo
df = Curva_di_nitrito_di_sodio #dataset con una colonna con i valori della X (Concentration) 
#e una colonna con i valori della variabile Y (Value)

#Se serve, seleziono le righe che mi servono:
df2 <- df[,] #specificare le righe, per es. dalla 1 alla 12 "df[c(1:12), ]


#FUNZIONE per la creazione del dataset 
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

######### Funzione da computare senza modifiche per la Regressione lineare
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


#Creazione dataset per la regressione lineare dal dataset originario
df3 <- data_summary(df2, #il dataset
                    varname="Value",               #nome della varibile risposta
                    groupnames=c("Concentration")) #vettore dei nomi di colonne che contengono i fattori

#Da usare in caso il vettore della variabile x non fosse classificato come numerico
df3$Concentration=as.numeric(df3$Concentration)
head(df3)

q <-  ggplot(df3, aes(x=Concentration,    #nome colonna Variabile indipendente
                      y=Value)) +         #nome colonna Variabile dipendente 
  geom_errorbar(aes(ymin=Value-sd,        #nome colonna Variabile dipendente
                    ymax=Value+sd),       #nome colonna Variabile dipendente
                width= 0.5, 
                size = 0.1,
                color ="black",
                position=position_dodge(0.1)) +
  geom_point(shape = 15, size = 3, color = "black") +   #forma, dimensione e colore dei punti
  stat_smooth(method = "lm", formula= y~x, color = "red", size =0.5) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
q

####
#NOTA BENE: in stat_regline_equation sono questi sono i parametri output:
#eq.label : equation for the fitted polynomial as a character string to be parsed
#rr.label : 𝑅2 of the fitted model as a character string to be parsed
#adj.rr.label : Adjusted 𝑅2 of the fitted model as a character string to be parsed
#AIC.label : AIC for the fitted model.
#BIC.label : BIC for the fitted model.
#hjust : Set to zero to override the default of the "text" geom.



