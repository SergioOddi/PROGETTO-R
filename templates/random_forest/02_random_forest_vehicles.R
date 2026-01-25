######## Random Forest sul dataset vehicles
## Febbraio 2021

library(dplyr)
library(ggplot2)
library(cowplot)
library(randomForest)

### carico il file ="vehicles.csv"
data <- read_csv(file.choose(), col_types ="nnnfnfffffnn")

### oppure... dopo aver caricato il file con comando Files
data <- vehicles

# Let's limit our dataset to the 1984 
data <- data %>%
  filter(year == "1983")

# Let's limit our dataset to alle classi Pickup e Vans 
data <- data %>%
  filter(class == "Pickup" | class == "Vans")

#rimuovo le variabili inutili
data <- data[, -c(6,7,9)] #tolgo variabili inutili

head(data)
str(data)

data$class <- ifelse(test=data$class == "Pickup", yes ="Pickup", no = "Vans")
data$class <- as.factor(data$class)
data$drive <- as.factor(data$drive)
data$transmissiontype <- as.factor(data$transmissiontype)
str(data)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(class ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# Per verificare quante variabili usare per costruire i nodi: ho una variabile Y (class) e 8 variabili esplicative
oob.values <- vector(length = 8)
for(i in 1:8) {
  temp.model <- randomForest(class ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.26; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(class ~ ., data = data.imputed, proximity = TRUE) # in questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Pickup", "Vans"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Pickup"],
          model$err.rate[, "Vans"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Poi si può ripetere il modello aumentando il numero degli alberi fino a che si vede che l'OOB si stabilizza: es. 1000, 2000 ecc.

model <- randomForest(class ~ ., data = data.imputed, ntree=1000, proximity = TRUE)
model

# Infine si può verificare se la scelta del numero di var. è corretta:
model <- randomForest(class ~ ., data = data.imputed, ntree=10000, mtry = 4, proximity = TRUE)
model

# In definitiva si può n. alberi 5000, n. variabili = 4


distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$class)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")
