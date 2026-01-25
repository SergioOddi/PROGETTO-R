
library(dplyr)
library(ggplot2)
library(cowplot)
library(randomForest)
library(stats)

data <- processed.cleveland #in data set R (Random Forest)
head(data)

colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)


data[data=="?"] <- NA
data[data$sex == 0, ]$sex <- "F"

data$sex <- recode_factor(data$sex, "1" = "M")
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes ="Healthy", no = "Unhealthy")
data$hd <- as.factor(data$hd)

str(data)

set.seed(42)

data.imputed <- rfImpute(hd ~ ., data =data, iter = 6)

# PER verificare quante variabili usare per costruire i nodi
oob.values <- vector(length = 10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data = data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
oob.values #vedi il numero più basso e il suo ordinale corrisponde al numero di variabili ottimali per la random Forest



model <- randomForest(hd ~ ., data = data.imputed, proximity = TRUE, ntree = 5000)
model



oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times =3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[, "OOB"],
          model$err.rate[, "Healthy"],
          model$err.rate[, "Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret= TRUE)
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label = Sample))+
  geom_text(aes(color=Status))+
  theme_bw()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))+
  ggtitle("MDS plot using (1- Random Forest Proximities)")



