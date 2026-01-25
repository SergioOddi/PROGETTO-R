######### Regressione lineare di curve multiple con media ed errori 
###### 21 maggio 2021

library(ggplot2)
library(tidyverse)
library(reshape2)

dati <- Time_Sheet2_2_

#reshaping del dataset
dati  <-  melt(dati, id.var ="Time") #riformatto il dataset in base ai fattori presenti nella colonna "Group"

#rename columns
dati = dati %>% rename("time" = "Time")

head(dati)

#### SOLO PUNTI
ggplot(dati, aes(x = time, y = value, group=variable, color=variable)) +
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
        panel.grid.minor = element_blank(), panel.background = element_blank())
  #stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red",  linetype="dotted")
  #stat_smooth(method = "loess", formula = y ~ x, size = 0.5, se = FALSE, colour = "black", linetype="solid")



##### CON CURVA
ggplot(dati, aes(x = time, y = value, group=variable, color=variable)) +
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
        panel.grid.minor = element_blank(), panel.background = element_blank())+
#stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red",  linetype="dotted")
stat_smooth(method = "loess", formula = y ~ x, size = 0.5, se = FALSE, colour = "black", linetype="solid")


##### CON CURVA che dico io...
ggplot(dati, aes(x = time, y = value, group=variable, color=variable)) +
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
        panel.grid.minor = element_blank(), panel.background = element_blank())+
  #stat_smooth(method = "loess", formula = y ~ x, size = 1, se = FALSE, colour = "red",  linetype="dotted")
  stat_smooth(method = "loess", formula = y ~ x, size = 0.5, se = FALSE, colour = "black", linetype="solid")






