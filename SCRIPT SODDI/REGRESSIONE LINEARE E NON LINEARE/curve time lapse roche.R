###### CURVE di Time lapse

#SITO ispiratore
# https://stackoverflow.com/questions/56508563/how-to-draw-three-differents-non-linear-regression-with-ggplot2


##FIGURA
##NUOVA LINEA congiungente punti con errori
#sito ispiratore: https://www.datanovia.com/en/blog/how-to-add-p-values-onto-basic-ggplots/

#LIBRERIE da utilizzare
library(ggplot2)
library(ggstatsplot)
library(ggpubr)
library(rstatix)
library(reshape2)

df <- Time_Foglio1_2_

#reshaping del dataset
dati_reshaped = melt(df, id.var ="Time") #riformatto il dataset in base ai fattori presenti nella colonna "Group"
df <- dati_reshaped

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
# to be summarized
# groupnames : vector of column names to be used as
# grouping variables
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

df2 <- data_summary(df, #il dataset
                    varname="value", #nome della varibile risposta
                    groupnames=c("Time")) #vettore dei nomi di colonne che contengono i fattori


# Disegna i punti e la retta che li collega
# Use geom_line()+geom_pointrange()
q = ggplot(df2, aes(x=Time, y=value, color="black")) + 
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, size = 0.25, 
                position=position_dodge(0.1)) +
  geom_line(aes(group = 1), size = 0.3) + #forma e dimensione della linea che congiunge i punti
  geom_point(shape = 15, size = 3)+ #forma e dimensione dei punti
  labs(title="",x="[URB597] (microM)", y = "FAAH activity (%)", color ="black")+
  theme_classic()
q + scale_color_grey() + 
  theme(axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
        axis.title=element_text(size=16,face=NULL, color ="black"), #setta le caratteristiche del titolo degli assi
        text = element_text(family = "Arial"))  ##setta il tipo di font
q


curva2 <- nls(value ~ (Ym * (1 - exp(-k*Time))), data = df2, start = list(Ym = 50, k = 1), trace = TRUE)


# DISEGNA solo i punti
q = ggplot(df2, aes(x=Time, y=value, color="black")) + 
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, size = 0.25, 
                position=position_dodge(0.1)) +
  #stat_smooth(method = 'nls', formula = 'y ~ Ym*(1-exp(-k*x))',                     #Fit nls model
  #            method.args = list(start=c(Ym = 50, k= 1)), se=FALSE, size =0.3) +
  #  geom_line(aes(group = 1), size = 0.3) + # forma e dimensione della linea che congiunge i punti
  geom_point(shape = 15, size = 3) + #forma e dimensione dei punti
  labs(title="",x="Time (min)", y = "Normalized Fluorescence (%)", color ="black")+
  theme_classic()
q + scale_color_grey() + 
  theme(axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
        axis.title=element_text(size=16,face=NULL, color ="black"), #setta le caratteristiche del titolo degli assi
        text = element_text(family = "Arial"))  ##setta il tipo di font
q





ggplot(df, aes(x = Time, y = value, group=variable, color=variable)) +
  stat_summary(aes(color = variable), fun.data="mean_sdl", fun.args = list(mult=1), geom="errorbar", position = "identity", size=0.5, width=0.2) +
  stat_summary(fun = "mean", geom = "point", size=3, aes(shape=variable,color=variable)) + 
  scale_x_discrete(name = "Time (min)") + #Titolo asse x
  scale_y_continuous(name = "Fluorescence (%)", limits=c(0, 70), breaks = seq(0, 4000,500)) + #Titolo asse x
  #scale_color_manual(values=c("green", "blue", "red"), name="Probe", labels=c("A","B","C")) + #Nome legenda
  #scale_shape_manual(values=c(15,16,17), name  ="Treatment", labels=c("A", "B", "C")) +
  ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + #Titolo asse y
  # theme(legend.position = "right") + 
  # theme(legend.background = element_rect(size=0.5, linetype="solid", color ="black", fill="white")) +
  theme(axis.line.x = element_line(size = 0.5, color = "black"),
        axis.text.x = element_text(color="black", size = 12),
        axis.line.y = element_line(size = 0.5, color = "black"),
        axis.text.y = element_text(color="black", size = 12),
        axis.title = element_text(size =15, face="bold"),
        plot.title = element_text(size =20, face = "bold"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  #stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red",  linetype="dotted")
  stat_smooth(method = "loess", formula = y ~ x, size = 0.5, se = FALSE, colour = "black", linetype="solid")










### Regressione nonlineare con un modello One-phase exponential association
### quando x è uguale a 0.693/k, Y = 0.5*Ym
curva2 <- nls(value ~ (Ym * (1 - exp(-k*Time))), data = df2, start = list(Ym = 50, k = 1), trace = TRUE)

##VISUALIZZA I DATI GREZZI
plot(value ~ Time, data = df2, ylim =c(0, 60), #intervallo delle ordinate
     ylab = "Normalized Fluorescence",
     xlab = Time  ~ (min))

summary(curva2)

coef(curva2)
Ym = coef(curva2)[1] ### Valore massimo di F
k = coef(curva2)[2]  ### costante di tempo 
abline(h = 0.85*Ym, lty = 2)

#Disegno la curva 
curve(Ym * (1 - exp(-k*x)), from = 0.0, to = 5, xlab="Time (s)", ylab="F", add = T, col ="red")

(250/200e-4)


