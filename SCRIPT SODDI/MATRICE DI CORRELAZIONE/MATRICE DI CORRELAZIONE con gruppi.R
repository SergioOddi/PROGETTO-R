###MATRICE Di correlazione con dati raggruppati
library(ggplot2)
library(GGally)

dati = Geni_HIPP_Topi_Tg_PEA_12_mesi_Exp_n_

# remove two levels to have only two groups
dati <- subset(dati, Genotype != "WT")
dati <- subset(dati, Genotype != "WT_U")

dati$Group <- factor(dati$Group)#

dati<- dati[, 4:13]

##Matrice di correlazione con dati raggruppati
ggpairs(dati, legend = 1, 
        mapping = ggplot2::aes(color=Group), size = 1, #qui occorre specificare la colonna dove sono i gruppi
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=1), 
                     discrete = "blank", combo="blank"), 
        diag = list(discrete="barDiag", 
                    continuous = wrap("densityDiag", alpha= 0.7, size = 0.1)), #size qui dà lo spessore della linea nella diagonale
        upper = list(continuous = wrap("cor", size = 3))) + #qui size cambia la dimensione dei coefficienti di correlazione
  theme(panel.grid.major = element_blank())    # remove gridlines 
