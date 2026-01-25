##### Random Forest su dati Boari
### 30 gennaio 2021


library(dplyr)
library(ggplot2)
library(cowplot)
library(randomForest)

data <- data_cani
data <- data[, c(2,6:12)]

head(data)

colnames(data) <- c(
  "Group",
  "sex",
  "age",
  "body_weight",
  "AEA",
  "AG2",
  "PEA",
  "OEA"
)


str(data)

data[data$sex == "FS", ]$sex <- "F"
data[data$sex == "MC", ]$sex <- "M"
data$sex <- as.factor(data$sex)

data.tot <- data


data <- data[c(31:39, 50:58), ]




data$Group <- ifelse(test=data$Group == "FRE", yes ="Healthy", no = "Unhealthy")
data$Group <- as.factor(data$Group)

str(data)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(Group ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# Per verificare quante variabili usare per costruire i nodi: ho una variabile Y (Group) e 7 variabili esplicative
oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(Group ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.2222222; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(Group ~ ., data = data.imputed, proximity = TRUE) # in questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Poi si può ripetere il modello aumentando il numero degli alberi fino a che si vede che l'OOB si stabilizza: es. 1000, 2000 ecc.

model <- randomForest(Group ~ ., data = data.imputed, ntree=5000, proximity = TRUE)
model

# Infine si può verificare se la scelta del numero di var. è corretta:
model <- randomForest(Group ~ ., data = data.imputed, ntree=500, mtry = 3, proximity = TRUE)
model

# In definitiva si può n. alberi 5000, n. variabili = 3


distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$Group)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")









####  Limitiamo l'analisi a sani e FRE

data <- data.tot
data <- data[c(1:40), ] #limito il dataset a sani e FRE

data$Group <- ifelse(test=data$Group == "healthy", yes ="Healthy", no = "Unhealthy")
data$Group <- as.factor(data$Group)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(Group ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# PER verificare quante variabili usare per costruire i nodi: ho una variabile Y (Group) e 7 variabili esplicative
oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(Group ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.2222222; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(Group ~ ., data = data.imputed, proximity = TRUE) # ini questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Poi si può ripetere il modello aumentando il numero degli alberi fino a che si vede che l'OOB si stabilizza: es. 1000, 2000 ecc.

model <- randomForest(Group ~ ., data = data.imputed, ntree=5000, proximity = TRUE)
model

# Infine si può verificare se la scelta del numero di var. è corretta:
model <- randomForest(Group ~ ., data = data.imputed, ntree=500, mtry = 2, proximity = TRUE)
model

# IN definitiva si può n. alberi 500, n. variabili = 2

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$Group)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")


####  Limitiamo l'analisi su sani e ARE

data <- data.tot
data <- data[c(1:30, 41:49), ] #limito il dataset a sani e ARE

data$Group <- ifelse(test=data$Group == "healthy", yes ="Healthy", no = "Unhealthy")
data$Group <- as.factor(data$Group)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(Group ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# PER verificare quante variabili usare per costruire i nodi: ho una variabile Y (Group) e 7 variabili esplicative
oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(Group ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.2222222; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(Group ~ ., data = data.imputed, proximity = TRUE) # ini questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Dal grafico si può verificare che la scelta automatica del numero di var. è corretta:
model <- randomForest(Group ~ ., data = data.imputed, ntree=500, mtry = 2, proximity = TRUE)
model

# In definitiva si possono usare n. alberi 500, n. variabili = 2

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$Group)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")


####  Infine Limitiamo l'analisi su sani e IBD/PLE

data <- data.tot
data <- data[c(1:30, 50:63), ] #limito il dataset a sani e FRE

data$Group <- ifelse(test=data$Group == "healthy", yes ="Healthy", no = "Unhealthy")
data$Group <- as.factor(data$Group)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(Group ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# PER verificare quante variabili usare per costruire i nodi: ho una variabile Y (Group) e 7 variabili esplicative
oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(Group ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.2222222; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(Group ~ ., data = data.imputed, proximity = TRUE) # ini questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Poi si può ripetere il modello aumentando il numero degli alberi fino a che si vede che l'OOB si stabilizza: es. 1000, 2000 ecc.
model <- randomForest(Group ~ ., data = data.imputed, ntree=5000, proximity = TRUE)
model


# IN definitiva si può n. alberi 5000, n. variabili = 2

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$Group)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")




#########
#### Conclusioni sembra che gli animali 1:3 24:36 e 27:30

data <- data.tot
data <- data[-c(1:3, 23:26), ] #limito il dataset a animali sani ben separati dai malati

data$Group <- ifelse(test=data$Group == "healthy", yes ="Healthy", no = "Unhealthy")
data$Group <- as.factor(data$Group)

set.seed(42)

data.imputed <- data

# data.imputed <- rfImpute(Group ~ ., data = data, iter = 6) ### NB: il dataset non ha valori NA e quindi questa procedura la salto

# PER verificare quante variabili usare per costruire i nodi: ho una variabile Y (Group) e 7 variabili esplicative
oob.values <- vector(length = 7)
for(i in 1:7) {
  temp.model <- randomForest(Group ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest
### in questo caso: 0.2222222; che è il terzo oob misurato con i=3, quindi tre variabili!


model <- randomForest(Group ~ ., data = data.imputed, proximity = TRUE) # ini questo modo il sistema automaticamente sceglie di analizzare 500 alberi; e di usare un numero di
#variabili pari alla radice quadrata del numero di variabili sqrt(7) = 2

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

# Poi si può ripetere il modello aumentando il numero degli alberi fino a che si vede che l'OOB si stabilizza: es. 1000, 2000 ecc.
model <- randomForest(Group ~ ., data = data.imputed, ntree=5000, mtry = 3, proximity = TRUE)
model



distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$Group)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")




