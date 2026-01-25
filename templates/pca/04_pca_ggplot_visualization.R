####### PCA center and scale con ggplot2
###

rm(list=ls(all=T))
graphics.off()


require(ggplot2)
require(ggbiplot)
library(stats)

#### Importare i dati
dati_completi <- Carluccio_data

#Selezionare i dati (vettore gruppi e vettori variabili)
dati <- dati_completi[, c(2:10)] 
dati=as.data.frame.list(dati)

pc <- prcomp(x=dati[, -1], 
             center = T,
             scale  = F)


sizeT=12


ggbiplot(pcobj = pc,
                 choices = c(1,2), ### which PCs to plot
                 scale = 1,      ###### covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance. 
                 obs.scale = 1, #### scale factor to apply to observations
                 var.scale = 1,  ##### scale factor to apply to variables
                 #labels = row.names(dati),
                 #pointshape = 1, pointsize = 5, labelsize = 5,
                 ################ Variabili ####################################
                 varname.abbrev = FALSE,  #####
                 varname.size = 2,        #####
                 varname.adjust = 1.5,    ##### 
                 var.axes = T,            #####
                 ###############################################################
                 ############### Circolo #######################################
                 circle = T,
                 circle.prob = 0.3,
                 ###############################################################
                 ############### Ellisse #######################################
                ellipse = TRUE, ellipse.prob = 0.95,
                alpha = 0.5,
                repel = TRUE,
                groups = dati$Group)+
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Arial"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 45, hjust = 1, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.1),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.1),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda "none" 
  )


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






