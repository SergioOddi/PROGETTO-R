#### ANALISI Dati PCR Domenico

##### ANALISI Macrofagi Domenico
#### 3 febbraio 2021

#PACKAGES usati
library(ggplot2)
library(factoextra)
library(FactoMineR)#per la funzione PCA
library(psych)
library(MASS)
library(parallel)
library(rela)
library(dplyr)

dati <- Dataset_Domenico_PCR_M1_M0_macrofagi_2_
dati <- dati[-22, ]

#rimuovo i macrofagi M1
dati <- subset(dati, Phenotype != "M1") #Healthy è il primo gruppo da rimuovere dal dataset

#recode_factor : rinomino il nome dei gruppi nella colonna "Group"
# recode_factor
#dati$Group <- recode_factor(dati$Group, "AD_M0" = "M0",
                            "AD_URB_M0" = "U_M0",
                            "AD_M1" = "M1",
                            "AD_URB_M1" = "U_M1")


#Seleziono le colonne di interesse
dati <- dati[, c(2, 5:14)] # Attenzione alla posizione dei vettori Group e variabili numeriche

dati.num <- dati[, -c(1)]

typeof(dati.num)
dati.num=as.data.frame.list(dati.num)
dati.num

options(scipen=999)#permit decimal values

pca=dati.num#Battezzo pca il data set con le colonne quantitative


pca <- as.matrix.data.frame(pca)





#####
dati.active <- pca#seleziono righe e variabili


res.pca = PCA(dati.active, graph = FALSE)
print(res.pca)

# Eigenvalues= autovalori= varianze per ogni PC
eig.val <- get_eigenvalue(res.pca)
eig.val

#Scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab ="PC")
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
             ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep ="")) # dà un plot con le prime due PC in cui ogni variabile appare come una frecia la cui punta
#è determinata dal valore di correlazione della variabile rispetto alla prima e alla seconda PC; più lunga la freccia maggiore è il contributo della variabile alla PC; è anche noto come plot di correlazione delle variabili: 
#la correlazione tra una variabile e una data PC è usata come le coordinare della variabile sulla PC
#mostra le relazioni tra le variabili: se sono dirette nel quadrante positivo e nella stessa direzione sono positivamente correlate tra loro; se una è posizionata nel lato opposto ad un'altra
#le due variabili sono correlate negativamente, se sono ortogonali tra loro sono indipendenti tra loro


res.desc <- dimdesc(res.pca, axes =c(1,2), proba = 0.05)#questa funzione serve per evidenziare le variabili più rilevanti per una data PC
res.desc$Dim.1#descrizione per la PC1 (Dim1)
res.desc$Dim.2#descrizione per la PC2 (Dim2)
res.desc$Dim.3#descrizione per la PC3 (Dim3)


# RISULTATI PER INDIVIDUI
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates per le singole unità
res.ind$contrib        # Contributions degli individui
res.ind$cos2           # cos2 per gli individui

#library("corrplot") #non si capisce da dove l'ho preso
#corrplot(var$cos2, is.corr=FALSE) #non si capisce da dove l'ho preso


#BIPLOT per la figura
#BIPLOT: per fare un  biplot di individui e variabili sul piano tra PC1 e PC2
fviz_pca_biplot(res.pca,
                geom.ind ="point",
                fill.ind = dati$Group, col.ind = "black",
                pointshape = 21, pointsize = 5, labelsize = 5,
                addEllipses = T, ellipse.type="confidence", ellipse.level = 0.95, #dimensione dell'ellisse in funzione 
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols ="Greys",
                repel = T,
                mean.point = F, #non fa vedere la media del gruppo
                xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
                ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""),
                legend.title = list(fill="Genotype", color = "Contrib"))

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
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(1,3))

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
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(2,3))

####3D plot
require(plotly)
#Mi servono le coordinate degli individui sulle prime tre PC (Dim)

datiPC <- cbind(dati, res.ind$coord[, 1:3]) #crea un data set con le prime 3 PC messe alla fine (colonne PC1, PC2 e PC3)
datiPC

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




###############################################
############
#### Analisi sugli M1

dati = Dataset_Domenico_PCR_M1_M0_macrofagi_2_
dati <- dati [-22, ]

#rimuovo i mcacrofagi M0

dati <- subset(dati, Phenotype!= "M0") 
#recode_factor : rinomino il nome dei gruppi nella colonna "Group"
# recode_factor
#dati$Group <- recode_factor(dati$Group, "CTRL_M0" = "cM0",
                            "CTRL_URB_M0" = "cU_M0",
                            "CTRL_M1" = "cM1",
                            "CTRL_URB_M1" = "cU_M1")

#Seleziono le colonne di interesse
dati <- dati[, c(2, 5:14)]

dati.num <- dati[, -c(1)]
typeof(dati.num)
dati.num=as.data.frame.list(dati.num)
dati.num

options(scipen=999)#permit decimal values

pca=dati.num#Battezzo pca il data set con le colonne quantitative
head(pca, 10)
tail(pca, 3)

pca <- as.matrix.data.frame(pca)


#####
dati.active <- pca#seleziono righe e variabili
dati.active

res.pca = PCA(dati.active, graph = FALSE)
print(res.pca)

# Eigenvalues= autovalori= varianze per ogni PC
eig.val <- get_eigenvalue(res.pca)
eig.val

#Scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab ="PC")
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
             ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep ="")) # dà un plot con le prime due PC in cui ogni variabile appare come una frecia la cui punta
#è determinata dal valore di correlazione della variabile rispetto alla prima e alla seconda PC; più lunga la freccia maggiore è il contributo della variabile alla PC; è anche noto come plot di correlazione delle variabili: 
#la correlazione tra una variabile e una data PC è usata come le coordinare della variabile sulla PC
#mostra le relazioni tra le variabili: se sono dirette nel quadrante positivo e nella stessa direzione sono positivamente correlate tra loro; se una è posizionata nel lato opposto ad un'altra
#le due variabili sono correlate negativamente, se sono ortogonali tra loro sono indipendenti tra loro


res.desc <- dimdesc(res.pca, axes =c(1,2), proba = 0.05)#questa funzione serve per evidenziare le variabili più rilevanti per una data PC
res.desc$Dim.1#descrizione per la PC1 (Dim1)
res.desc$Dim.2#descrizione per la PC2 (Dim2)
res.desc$Dim.3#descrizione per la PC3 (Dim3)


# RISULTATI PER INDIVIDUI
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates per le singole unità
res.ind$contrib        # Contributions degli individui
res.ind$cos2           # cos2 per gli individui

#library("corrplot") #non si capisce da dove l'ho preso
#corrplot(var$cos2, is.corr=FALSE) #non si capisce da dove l'ho preso


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
                legend.title = list(fill="Genotype", color = "Contrib"))

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
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(1,3))

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
                legend.title = list(fill="Genotype", color = "Contrib"), axes=c(2,3))



#BIPLOT: per fare un  biplot di individui e variabili
fviz_pca_biplot(res.pca,
                geom.ind ="point",
                fill.ind = dati$Group, col.ind = "black",
                pointshape = 21, pointsize =2,
                addEllipses = T, ellipse.type="confidence", #dimensione dell'ellisse in funzione 
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols ="Greys",
                repel = T, #non sovrapporre il testo
                mean.point = F, #non fa vedere la media del gruppo
                xlab=paste("PC1 (", round(res.pca$eig[1,2],1),"%)", sep =""), 
                ylab=paste("PC2 (", round(res.pca$eig[2,2],1),"%)", sep =""),
                legend.title = list(fill="Genotype", color = "Contrib"))


#si può usare quando ci sono pochi individui e variabili
#un individuo che è sullo stesso lato di una data variabile ha un alto valore per quella variabile
#un individuo che sia sul lato opposto di una data variabile ha un basso valore per quella variabile


####3D plot
require(plotly)
#Mi servono le coordinate degli individui sulle prime tre PC (Dim)

datiPC <- cbind(dati, res.ind$coord[, 1:3]) #crea un data set con le prime 3 PC messe alla fine (colonne PC1, PC2 e PC3)
datiPC

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
