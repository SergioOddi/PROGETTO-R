##### HEATMAP using Pheatmap 
#### DATI MARINA
#### 3 febbraio 2021

###tratto da https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/

# load package
library("pheatmap")
library("dendextend")

# load data and subset
dati = marina

# Considero solo i CTRL (to have only 2 groups)
dati <- subset(dati, Group == "CTRL_M0" | Group == "CTRL_M1")

dati$Group <- factor(dati$Group)#Trasforma i dati in Group in fattori!
dati

dati <- dati[, c(1, 2, 6:11)] #Prendo le IDs, il Group e le variabili numeriche

typeof(dati)
dati=as.data.frame.list(dati)#il dataframe è ora una lista
dati

rownames(dati) <- dati[,1] #Assigning row names from 1st column
dati
dati[,1] <- NULL #Removing la colonna ID, non più necessaria
dati

dati_num = dati[, -c(1)]#Rimuovo la colonna Treatment per avere solo colonne numeriche
dati_num

df <- scale(dati_num)#scalo i dati numerici
df

# create heatmap using pheatmap
#la cosa comoda è che si può dare al sistema il nome dei gruppi usando un altro dataset...

dati2=dati[,]
dati2
pheatmap(df[,], annotation_row = dati2[, 1, drop=FALSE]) ## si noti che 1 indica la colonna Treatment

#Now before I demonstrate the main functionality that I like so much about pheatmap, 
#which is creating annotations, we need to figure out how we would like to colour the rows. 
#I’ll perform hierarchical clustering in the same manner as performed by pheatmap to obtain gene clusters. 
#I use the excellent dendextend to plot a simple dendrogram.

my_hclust_gene <- hclust(dist(df), method = "complete")

# load package
library(dendextend)

as.dendrogram(my_hclust_gene) %>%
  plot(horiz = TRUE)



#tratto da https://stats.stackexchange.com/questions/397062/hierarchical-clustering-dendrogram-on-a-distance-matrix

# Make a heatmap
p1 <- pheatmap(df[,], annotation_row = dati[,1, drop=FALSE])


#Another function that I like about pheatmap is the ability to introduce breaks in the heatmap. 
#I’ll break up the heatmap by specifying how many clusters I want from the dendrograms. 
#(You can also manually define where you want breaks too.)

pheatmap(df[,],
         annotation_row = dati2[,1, drop=FALSE], #NOTA BENE:si può anche mettere "annotation_col = my_sample_col"
         cutree_rows = 2,
         cutree_cols = 2)

pheatmap(df[,], annotation_row = dati2[,1, drop=FALSE])














# load data and subset
dati = marina

# Considero solo i CTRL (to have only 2 groups)
dati <- subset(dati, Group == "CTRL_M0" | Group == "CTRL__URB_M0")

dati$Group <- factor(dati$Group)#Trasforma i dati in Group in fattori!
dati

dati <- dati[, c(1, 2, 6:11)] #Prendo le IDs, il Group e le variabili numeriche

typeof(dati)
dati=as.data.frame.list(dati)#il dataframe è ora una lista
dati

rownames(dati) <- dati[,1] #Assigning row names from 1st column
dati
dati[,1] <- NULL #Removing la colonna ID, non più necessaria
dati

dati_num = dati[, -c(1)]#Rimuovo la colonna Treatment per avere solo colonne numeriche
dati_num

df <- scale(dati_num)#scalo i dati numerici
df

# create heatmap using pheatmap
#la cosa comoda è che si può dare al sistema il nome dei gruppi usando un altro dataset...

dati2=dati[,]
dati2
pheatmap(df[,], annotation_row = dati2[, 1, drop=FALSE]) ## si noti che 1 indica la colonna Treatment

my_hclust_gene <- hclust(dist(df), method = "complete")

# load package
library(dendextend)

as.dendrogram(my_hclust_gene) %>%
  plot(horiz = TRUE)


#tratto da https://stats.stackexchange.com/questions/397062/hierarchical-clustering-dendrogram-on-a-distance-matrix
# Make a heatmap
p1 <- pheatmap(df[,], annotation_row = dati[,1, drop=FALSE])


pheatmap(df[,],
         annotation_row = dati2[,1, drop=FALSE], #NOTA BENE:si può anche mettere "annotation_col = my_sample_col"
         cutree_rows = 2,
         cutree_cols = 2)

pheatmap(df[,], annotation_row = dati2[,1, drop=FALSE])





# load data and subset
dati = marina

# Considero solo i CTRL (to have only 2 groups)
dati <- subset(dati, Group == "CTRL_M1" | Group == "CTRL__URB_M1")

dati$Group <- factor(dati$Group)#Trasforma i dati in Group in fattori!
dati

dati <- dati[, c(1, 2, 6:11)] #Prendo le IDs, il Group e le variabili numeriche

typeof(dati)
dati=as.data.frame.list(dati)#il dataframe è ora una lista
dati

rownames(dati) <- dati[,1] #Assigning row names from 1st column
dati
dati[,1] <- NULL #Removing la colonna ID, non più necessaria
dati

dati_num = dati[, -c(1)]#Rimuovo la colonna Treatment per avere solo colonne numeriche
dati_num

df <- scale(dati_num)#scalo i dati numerici
df

# create heatmap using pheatmap
#la cosa comoda è che si può dare al sistema il nome dei gruppi usando un altro dataset...

dati2=dati[,]
dati2
pheatmap(df[,], annotation_row = dati2[, 1, drop=FALSE]) ## si noti che 1 indica la colonna Treatment

my_hclust_gene <- hclust(dist(df), method = "complete")

# load package
library(dendextend)

as.dendrogram(my_hclust_gene) %>%
  plot(horiz = TRUE)


#tratto da https://stats.stackexchange.com/questions/397062/hierarchical-clustering-dendrogram-on-a-distance-matrix
# Make a heatmap
p1 <- pheatmap(df[,], annotation_row = dati[,1, drop=FALSE])


pheatmap(df[,],
         annotation_row = dati2[,1, drop=FALSE], #NOTA BENE:si può anche mettere "annotation_col = my_sample_col"
         cutree_rows = 2,
         cutree_cols = 2)

pheatmap(df[,], annotation_row = dati2[,1, drop=FALSE])





