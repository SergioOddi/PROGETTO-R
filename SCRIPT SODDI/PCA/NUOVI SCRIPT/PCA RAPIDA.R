######## PCA RAPIDA

######## Versione 1.0
######## 12 giugno 2021

rm(list=ls(all=T))
graphics.off()

#PACKAGES usati
library(ggplot2)
library(FactoMineR)   # per la funzione PCA
library("factoextra") # per la funzione "get_eigenvalue"

options(scipen=999) # esprime i numeri in formato fisso

dati <- Dataset_Citokine_Array_Topi_TG_Veh_PEA_Citochine_n_3


# dataset classico, con ogni variabile per colonna

################ Scelta delle osservazioni e delle variabili dsa usare
#dati <- subset(dati, Group != "ctrl")
dati <- subset(dati, TIMEPOINT == "1")
dati <- subset(dati, GROUP != "probiotico")
dati <- subset(dati, GROUP != "pre-pro")

dati_tot <- dati
#####   Seleziono il vettore Group e le variabili
dati <- dati[, -c(1:2)] #Prendo solo la colonna gruppi e le colonne con le variabili

dati <- dati[, -c(1)]

############ Outlier
#Se serve rimuovere oulier
#dati = dati[-38, ]

########## Rinominare colonne
#Se serve rinominare colonne
dati <- dati %>% rename("Group" = "GROUP")

#### Rinominare livelli delle variabili categoriali
######## Se occorre rinominare un livello di un vettore categoriale (es., GROUP)
#dati$GROUP <- recode_factor(dati$GROUP, "nome da sostituire 1" = "nome corretto 1"
#                                      , "nome da sostituire 2" = "nome corretto 2"
#                                                                                 )

####### Eliminare/selezionare gruppi
### Se serve rimuovere osservazioni falsate
#dati <- subset(dati, Group != "ctrl")
#dati <- subset(dati, Group == "ctrl" | Group == "AD MODERATE")

##########################################################

### Dataset con le sole variabili numeriche
dati.num <- dati[, -c(1)]


####### PCA
res.pca = PCA(dati.num, scale.unit = T, graph = FALSE)

# Eigenvalues= autovalori= varianze per ogni PC
eig.val <- get_eigenvalue(res.pca)

#######1. SCREE PLOT
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab ="PC")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

####### Estrazione dei risultati per le variabili 
#Il comando get_pca_var(res.pca) mi dà una lista di matrici con tutti i risulati delle variabili attive (coordinate, correlazioni tra variabili e assi, coseni quadrati e contributi)
res.var = get_pca_var(res.pca)

######2. CERCHIO di correlazione LOADING PLOT
fviz_pca_var(res.pca, col.var ="coord", 
             gradient.cols =c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
             ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

######## RISULTATI PER INDIVIDUI
res.ind <- get_pca_ind(res.pca)

#####3a. BIPLOT PC1/PC2: per fare un  biplot di individui e variabili sul piano tra PC1 e PC2
fviz_pca_biplot(res.pca,
                geom.ind ="point",
                fill.ind = dati$Group, col.ind = "black",
                pointshape = 21, pointsize = 3, labelsize = 5,
                addEllipses = T, ellipse.type="confidence", ellipse.level = 0.95, #dimensione dell'ellisse in funzione 
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols ="Greys",
                repel = T,
                mean.point = F, #non fa vedere la media del gruppo
                xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
                ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""),
                legend.title = list(fill="Genotype", color = "Contrib"))+
  labs(title="") + #Per togliere il titolo del biplot
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key= element_blank(),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right", ##### Se non si vuole la legenda "none" 
        axis.line = element_line(colour = "black"))

###### 3b. BIPLOT PC1/PC3: per fare un  biplot di individui e variabili sul piano tra PC1 e PC3
fviz_pca_biplot(res.pca,
                geom.ind ="point",
                fill.ind = dati$Group, col.ind = "black",
                pointshape = 21, pointsize = 3, labelsize = 5,
                addEllipses = T, ellipse.type="confidence", ellipse.level = 0.95, #dimensione dell'ellisse in funzione 
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols ="Greys",
                repel = T,
                mean.point = F, #non fa vedere la media del gruppo
                xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
                ylab=paste("PC3 (", round(res.pca$eig[3,2],1),"%)", sep =""),
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(1,3))+
  labs(title="") + #Per togliere il titolo del biplot
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key= element_blank(),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right", ##### Se non si vuole la legenda "none" 
        axis.line = element_line(colour = "black"))

###### 3c. BIPLOT PC2/PC3: per fare un  biplot di individui e variabili sul piano tra PC2 e PC3
fviz_pca_biplot(res.pca,
                geom.ind ="point",
                fill.ind = dati$Group, col.ind = "black",
                pointshape = 21, pointsize = 3, labelsize = 5,
                addEllipses = T, ellipse.type="confidence", ellipse.level = 0.95, #dimensione dell'ellisse in funzione 
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols ="Greys",
                repel = T,
                mean.point = F, #non fa vedere la media del gruppo
                xlab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""), 
                ylab=paste("PC3 (", round(res.pca$eig[3,2],1),"%)", sep =""),
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(2,3))+
  labs(title="") + #Per togliere il titolo del biplot
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=16, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=16, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key= element_blank(),
        #legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right", ##### Se non si vuole la legenda "none" 
        axis.line = element_line(colour = "black"))




