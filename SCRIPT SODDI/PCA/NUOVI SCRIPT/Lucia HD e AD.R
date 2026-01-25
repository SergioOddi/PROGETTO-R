######## ANALISI PCA - Macrofagi Lucia
######## Sani e AD_MILD 
######## 30 maggio 2025

rm(list=ls(all=T))
graphics.off()

#PACKAGES usati
library(dplyr)
library(ggplot2)
library(FactoMineR) # per la funzione PCA
library("factoextra") # per la funzione "get_eigenvalue"



options(scipen=999) # esprime i numeri in formato fisso


dati <- Lucia# dataset classico, con ogni variabile per colonna

########## Rinominare colonne
#Se serve rinominare colonne
#dati <- dati %>% rename("Group" = "Treatment")
#head(dati)



####### Eliminare/selezionare gruppi
### Se serve rimuovere osservazioni falsate
dati <- subset(dati, Group != "HD_M0")
dati <- subset(dati, Group != "ADmild_M0")
#dati <- subset(dati, Subject == "AD_MILD")
#dati <- subset(dati, Subject == "HD" | Group == "MCI_PEA_T1")
#dati_time1 <- subset(dati, Timepoint == "Timepoint 1")

#############################

#dati <- dati[-37,]

################ Scelta delle osservazioni e delle variabili dsa usare

#####   Seleziono il vettore Group e le variabili
dati <- dati[, c(4:17)] #Prendo solo la colonna gruppi e le colonne con le variabili

#dati <- dati[, -c(6:9)]

############ Outlier
#Se serve rimuovere oulier
#dati <- dati[-c(14, 16, 26),]


########## Rinominare colonne
#Se serve rinominare colonne
#dati <- dati %>% rename("Group" = "Dieta")
#head(dati)



dati$Group <- as.factor(dati$Group)
str(dati)

### Selezionare il dataset delle sole variabili numeriche
dati.num <- dati[, -c(1)]
head(dati.num) 

##### PCA ---- vedere tipologia di PCA...
res.pca  <-  PCA(dati.num, scale.unit = T, graph = FALSE)

# Eigenvalues= autovalori= varianze per ogni PC
eig.val <- get_eigenvalue(res.pca)
eig.val

#Scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab ="PC")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
#scree plot con le % con cui le varie PC contribuiscono alla variazione: quante dimensioni servono per descrivere i dati?
#tipicamente il cut-off è il 10%, le PC con autovalori(explained variances) maggiori del 10% sono mantenute, le altre scartate; se invece siamo intenzionati a preservare la massima informazione possiamo decidere di
#prendere il numero di PC che cumulativamente spiegano una certa percentuale della varianza totale (p.e. almeno l'80%)

#Estrazione dei risultati per le variabili 
#Il comando get_pca_var(res.pca) mi dà una lista di matrici con tutti i risulati delle variabili attive (coordinate, correlazioni tra variabili e assi, coseni quadrati e contributi)
res.var = get_pca_var(res.pca)

# Results for Variables
res.var$coord          # Coordinate per le variabili
res.var$cos2           # Quality of representation: esprime la qualità della rappresentazione per le variabili su una mappa di fattori(?) è calcolato come coordinate al quadrato 
res.var$contrib        # Contributions (in %) delle variabili to the PCs

#CERCHIO di correlazione LOADING PLOT
head(res.var$coord, 2)

fviz_pca_var(res.pca, col.var ="coord", 
             gradient.cols =c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
             ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# dà un plot con le prime due PC in cui ogni variabile appare come una frecia la cui punta
#è determinata dal valore di correlazione della variabile rispetto alla prima e alla seconda PC; più lunga la freccia maggiore è il contributo della variabile alla PC; è anche noto come plot di correlazione delle variabili: 
#la correlazione tra una variabile e una data PC è usata come le coordinare della variabile sulla PC
#mostra le relazioni tra le variabili: se sono dirette nel quadrante positivo e nella stessa direzione sono positivamente correlate tra loro; se una è posizionata nel lato opposto ad un'altra
#le due variabili sono correlate negativamente, se sono ortogonali tra loro sono indipendenti tra loro


res.desc <- dimdesc(res.pca, axes =c(1,2), proba = 0.05)#questa funzione serve per evidenziare le variabili più rilevanti per una data PC
res.desc$Dim.1#descrizione per la PC1 (Dim1)
res.desc$Dim.2#descrizione per la PC2 (Dim2)

res.desc <- dimdesc(res.pca, axes =c(1,3), proba = 0.05)
res.desc$Dim.3#descrizione per la PC3 (Dim3)


# RISULTATI PER INDIVIDUI
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates per le singole unità
res.ind$contrib        # Contributions degli individui
res.ind$cos2           # cos2 per gli individui


#BIPLOT per la figura
#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC1 e PC2
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
                legend.title = list(fill="Group", color = "Contrib"))+
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

#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC1 e PC3
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
                legend.title = list(fill="Group", color = "Contrib"), axes=c(1,3))+
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

#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC2 e PC3
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
                legend.title = list(fill="Group", color = "Contrib"), axes=c(2,3))+
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

####3D plot
require(plotly)
#Mi servono le coordinate degli individui sulle prime tre PC (Dim)

datiPC <- cbind(dati, res.ind$coord[, 1:3]) #crea un data set con le prime 3 PC messe alla fine (colonne PC1, PC2 e PC3)
datiPC

#Creo un grafico 2D con il nuovo data set
plot_ly(datiPC, x = ~Dim.1, y = ~Dim.2, color = as.factor(datiPC$Group))

#Creo un grafico 3D con il nuovo data set
p <- plot_ly(datiPC, x = ~Dim.1, y = ~Dim.2, z =~Dim.3, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter3d", mode ="markers", 
             color = as.factor(datiPC$Group)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "3D scatter plot",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ))
p


###################################
##################################
##################################
### SOGGETTI: AD. Gruppi M1 e M1_U



#######
dati <- DATA_SET_Human_macrophages_rayplex # dataset classico, con ogni variabile per colonna

########## Rinominare colonne
#Se serve rinominare colonne
dati <- dati %>% rename("Group" = "Treatment")
#head(dati)



####### Eliminare/selezionare gruppi
### Se serve rimuovere osservazioni falsate
#dati <- subset(dati, Group != "AD SEVERE")
#dati <- subset(dati, Subject == "HD")
dati <- subset(dati, Subject == "AD_MILD")
dati <- subset(dati, Group != "M0")
#dati_time1 <- subset(dati, Timepoint == "Timepoint 1")

#############################

################ Scelta delle osservazioni e delle variabili dsa usare

#####   Seleziono il vettore Group e le variabili
dati <- dati[, c(3:16)] #Prendo solo la colonna gruppi e le colonne con le variabili

#dati <- dati[, -c(6:9)]

############ Outlier
#Se serve rimuovere oulier
#dati <- dati[-c(14, 16, 26),]


########## Rinominare colonne
#Se serve rinominare colonne
#dati <- dati %>% rename("Group" = "Dieta")
#head(dati)


dati$Group <- as.factor(dati$Group)
str(dati)

### Selezionare il dataset delle sole variabili numeriche
dati.num <- dati[, -c(1)]
head(dati.num) 

##### PCA ---- vedere tipologia di PCA...
res.pca  <-  PCA(dati.num, scale.unit = T, graph = FALSE)

# Eigenvalues= autovalori= varianze per ogni PC
eig.val <- get_eigenvalue(res.pca)
eig.val

#Scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab ="PC")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
#scree plot con le % con cui le varie PC contribuiscono alla variazione: quante dimensioni servono per descrivere i dati?
#tipicamente il cut-off è il 10%, le PC con autovalori(explained variances) maggiori del 10% sono mantenute, le altre scartate; se invece siamo intenzionati a preservare la massima informazione possiamo decidere di
#prendere il numero di PC che cumulativamente spiegano una certa percentuale della varianza totale (p.e. almeno l'80%)

#Estrazione dei risultati per le variabili 
#Il comando get_pca_var(res.pca) mi dà una lista di matrici con tutti i risulati delle variabili attive (coordinate, correlazioni tra variabili e assi, coseni quadrati e contributi)
res.var = get_pca_var(res.pca)

# Results for Variables
res.var$coord          # Coordinate per le variabili
res.var$cos2           # Quality of representation: esprime la qualità della rappresentazione per le variabili su una mappa di fattori(?) è calcolato come coordinate al quadrato 
res.var$contrib        # Contributions (in %) delle variabili to the PCs

#CERCHIO di correlazione LOADING PLOT
head(res.var$coord, 2)

fviz_pca_var(res.pca, col.var ="coord", 
             gradient.cols =c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
             ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# dà un plot con le prime due PC in cui ogni variabile appare come una frecia la cui punta
#è determinata dal valore di correlazione della variabile rispetto alla prima e alla seconda PC; più lunga la freccia maggiore è il contributo della variabile alla PC; è anche noto come plot di correlazione delle variabili: 
#la correlazione tra una variabile e una data PC è usata come le coordinare della variabile sulla PC
#mostra le relazioni tra le variabili: se sono dirette nel quadrante positivo e nella stessa direzione sono positivamente correlate tra loro; se una è posizionata nel lato opposto ad un'altra
#le due variabili sono correlate negativamente, se sono ortogonali tra loro sono indipendenti tra loro


res.desc <- dimdesc(res.pca, axes =c(1,2), proba = 0.05)#questa funzione serve per evidenziare le variabili più rilevanti per una data PC
res.desc$Dim.1#descrizione per la PC1 (Dim1)
res.desc$Dim.2#descrizione per la PC2 (Dim2)

res.desc <- dimdesc(res.pca, axes =c(1,3), proba = 0.05)
res.desc$Dim.3#descrizione per la PC3 (Dim3)


# RISULTATI PER INDIVIDUI
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates per le singole unità
res.ind$contrib        # Contributions degli individui
res.ind$cos2           # cos2 per gli individui


#BIPLOT per la figura
#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC1 e PC2
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
                legend.title = list(fill="Group", color = "Contrib"))+
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

#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC1 e PC3
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
                legend.title = list(fill="Group", color = "Contrib"), axes=c(1,3))+
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

#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC2 e PC3
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
                legend.title = list(fill="Group", color = "Contrib"), axes=c(2,3))+
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

####3D plot
require(plotly)
#Mi servono le coordinate degli individui sulle prime tre PC (Dim)

datiPC <- cbind(dati, res.ind$coord[, 1:3]) #crea un data set con le prime 3 PC messe alla fine (colonne PC1, PC2 e PC3)
datiPC

#Creo un grafico 2D con il nuovo data set
plot_ly(datiPC, x = ~Dim.1, y = ~Dim.2, color = as.factor(datiPC$Group))

#Creo un grafico 3D con il nuovo data set
p <- plot_ly(datiPC, x = ~Dim.1, y = ~Dim.2, z =~Dim.3, #indicare a quale colonna del data set corrisponde la x, la y e la z (in questo caso, Dim.1, ecc.)
             type = "scatter3d", mode ="markers", 
             color = as.factor(datiPC$Group)) %>% #devo indicare il nome della colonna qualitativa (Genotype)
  layout(
    title = "3D scatter plot",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ))
p

















