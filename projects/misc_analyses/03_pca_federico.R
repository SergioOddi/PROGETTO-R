#caricare la libreria
library(mdatools)


of <- dati


#Dividere il data set in variabili e qualifiche 
OF<-data.matrix(of[2:5]) #### variabili
OFV<-as.matrix(of[1])
OF
OFV

#Creare la PCA
m<-pca(OF, 4, scale = TRUE, info = "xxxx")  ##### Numero di ...
m<-selectCompNum(m, 21)                     ##### Numeto di componenti 

#Vedere  valori della PCA
print(m)
#Vedere la Varianza spiegata
m$loadings
m$eigenvals
mdaplot(m$eigenvals, type = "h")
plotVariance(m$res$cal, type = "h", show.labels = TRUE, labels = "values")
plotCumVariance(m$res$cal, type = "h", show.labels = TRUE, labels = "values")

#Creare i fattori variabili
g1<-factor(OFV[,"Mese"])
g2<-factor(OFV[, "specie"])
g3<-factor(OFV[, "Sample"])
g <- interaction(g1, g2)

#GRAFICI
#score plot
par(mfrow = c(2, 2))
p<-plotScores(m$res$cal, c(1, 2), cgroup = g2)
plotConfidenceEllipse(p)
#loading plot
mdaplot(m$loadings, type = "p", show.labels = TRUE, show.lines = c(0,0))
#biplot
plotBiplot(m, c(1:2), pch = c(16,NA), show.labels = T, show.excluded = T)
#Vedere i residui e gli outliers
plotResiduals(m$res$cal, show.labels = TRUE, labels = g3)
plotResiduals(m, ncomp = 2, main = "Distances for model (2 Comp)", show.labels = TRUE, labels = g3)

g2