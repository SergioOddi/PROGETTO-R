######### t-SNE

#### Studio 12 giugno 2021

library(Rtsne)

library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)
library(tsne)



####### Dataset Boari

dati <- data_cani
dati <- dati[, c(2, 10:13)]

dati$Group <- recode_factor(dati$Group, "healthy" = "1",
                            "FRE" = "2",
                            "ARE" = "3",
                            "IBD"= "4")
dati <- subset(dati, Group != "PLE")

## Curating the database for analysis with both t-SNE and PCA
dati$Group <- as.factor(dati$Group)

## for plotting
colors = rainbow(length(unique(dati$Group)))
names(colors) = unique(dati$Group)

per = 4

## Executing the algorithm on curated data
tsne <- Rtsne(dati[,-1], 
              dims = 2,  
              pca = T, 
              perplexity= per, #### tipicamente da 5 a 50
              check_duplicates = FALSE, 
              verbose=TRUE, 
              max_iter = 5000, 
              theta=0.5   #### accuratezza: max = 0, min = 1
              )
exeTimeTsne<- system.time(Rtsne(dati[,-1], dims = 2,  
                                pca = T, 
                                perplexity = per, 
                                check_duplicates = FALSE, 
                                verbose=TRUE, 
                                max_iter = 5000, 
                                theta=0.5))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=dati$Group, col=colors[dati$Group])

###plot T-SNE
tsne.plot = ggplot(as.data.frame(tsne[["Y"]]), mapping = aes(x = V1, y = V2,
                                                             col = dati$Group))+
  geom_point()+
  labs(x = 't-SNE Dimension 1', y = 't-SNE Dimension 2', title = 't-SNE')
tsne.plot 


###### Esempio con Iris

#### t-SNE
iris$Species <- recode_factor(iris$Species, "setosa" = "1",
                            "versicolor" = "2",
                            "virginica" = "3")
## for plotting
dev.new()
colors = rainbow(length(unique(iris$Species)))
names(colors) = unique(iris$Species)

## Executing the algorithm on curated data
tsne <- Rtsne(iris[,1:4], dims = 2,  check_duplicates = FALSE, pca = T, perplexity=30, verbose=TRUE, max_iter = 1000, 
              theta=0.5)
exeTimeTsne<- system.time(Rtsne(iris[,1:4], dims = 2,  check_duplicates = FALSE, pca = T, perplexity = 30, verbose=TRUE, max_iter = 1000, theta=0.5))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=iris$Species, col=colors[iris$Species])

###plot T-SNE
tsne.plot = ggplot(as.data.frame(tsne[["Y"]]), mapping = aes(x = V1, y = V2,
                                                             col = iris$Species))+
  geom_point()+
  labs(x = 't-SNE Dimension 1', y = 't-SNE Dimension 2', title = 't-SNE')
tsne.plot 


#### PCA
# compare to PCA
dev.new()
pca_iris = princomp(iris[,1:4])$scores[,1:2]
plot(pca_iris, t='n')
text(pca_iris, labels=iris$Species,col=colors[iris$Species])

#######################################################



####### Dataset Carluccio

dati <- Carluccio_data
dati <- dati[, c(2:10,16)]

dati$Group <- recode_factor(dati$Group, "GF" = "1",
                            "PF" = "2")

dati$Group <- as.factor(dati$Group)
## for plotting
colors = rainbow(length(unique(dati$Group)))
names(colors) = unique(dati$Group)

## Executing the algorithm on curated data
tsne <- Rtsne(dati[,-1], 
              dims = 2,  
              pca = F, 
              perplexity= 3, #### tipicamente da 5 a 50
              check_duplicates = FALSE, 
              verbose=TRUE, 
              max_iter = 5000, 
              theta=0   #### accuratezza: max = 0, min = 1
)
exeTimeTsne<- system.time(Rtsne(dati[,-1], dims = 2,  pca = F, perplexity = 3, check_duplicates = FALSE, verbose=TRUE, max_iter = 5000, theta=0))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=dati$Group, col=colors[dati$Group])

###plot T-SNE
tsne.plot = ggplot(as.data.frame(tsne[["Y"]]), mapping = aes(x = V1, y = V2,
                                                             col = dati$Group))+
  geom_point()+
  labs(x = 't-SNE Dimension 1', y = 't-SNE Dimension 2', title = 't-SNE')
tsne.plot 





####### Dataset soggetti Alzheimer

dati <- PLASMA_ECS_level_Foglio1
dati <- dati[-38, c(2,13:16)]

#Se serve rinominare colonne
dati <- dati %>% rename("Group" = "GROUP")

dati$Group <- recode_factor(dati$Group, "ctrl" = "1",
                            "AD MILD" = "2",
                            "AD MODERATE"= "3")

dati <- subset(dati, Group != "AD SEVERE")
dati <- subset(dati, Group != "2")

dati$Group <- as.factor(dati$Group)
## for plotting
colors = rainbow(length(unique(dati$Group)))
names(colors) = unique(dati$Group)

per = 10

## Executing the algorithm on curated data
tsne <- Rtsne(dati[,-1], 
              dims = 2,  
              pca = T, 
              perplexity= per, #### tipicamente da 5 a 50
              check_duplicates = FALSE, 
              verbose=TRUE, 
              max_iter = 1000, 
              theta=0.5   #### accuratezza: max = 0, min = 1
)
exeTimeTsne<- system.time(Rtsne(dati[,-1], dims = 2,  
                                pca = T,
                                perplexity = per, 
                                check_duplicates = FALSE, 
                                verbose=TRUE, 
                                max_iter = 1000, 
                                theta=0.5))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=dati$Group, col=colors[dati$Group])


####### Dataset topi PEA

dati <- Campioni_sangue_Topi_FINALIZZATA_2nd_tornata
dati <- subset(dati, Tornata != "Prima")
dati <- dati[, c(5:9)]


dati$Group <- recode_factor(dati$Group, "Placebo" = "1",
                            "PEA" = "2")

dati$Group <- as.factor(dati$Group)
## for plotting
colors = rainbow(length(unique(dati$Group)))
names(colors) = unique(dati$Group)

per = 5

## Executing the algorithm on curated data
tsne <- Rtsne(dati[,-1], 
              dims = 2,  
              pca = F, 
              perplexity= per, #### tipicamente da 5 a 50
              check_duplicates = FALSE, 
              verbose=TRUE, 
              max_iter = 100000, 
              theta=1   #### accuratezza: max = 0, min = 1
)
exeTimeTsne<- system.time(Rtsne(dati[,-1], dims = 2,  
                                pca = F,
                                perplexity = per, 
                                check_duplicates = FALSE, 
                                verbose=TRUE, 
                                max_iter = 100000, 
                                theta=1))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=dati$Group, col=colors[dati$Group])


