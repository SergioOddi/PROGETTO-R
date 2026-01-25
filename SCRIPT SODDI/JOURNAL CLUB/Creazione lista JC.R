# Creazione Lista JC

Nomi <- c("Sergio Oddi",
          "Annarita Lizzi",
          "Francesca Ciaramellano",
          "Alessandro Leuti",
          "Sara Standoli",
          "Veronica Carnicelli",
          "Clotilde Angelucci",
          "Camilla di Meo",
          "Carla Luzzi",
          "Cinzia Rapino",
          "Daniel Tortolani",
          "Domenico Fazio",
          "Federico Fanti",
          "Lucia Scipioni",
          "Roberto Giacominelli",
          "Marina Fava",
          "Mauro Maccarrone")

n = length(nomi)

mydata <- c(1:n)
Ordine <- mydata[sample(mydata, n,
                          replace=FALSE)]

table <- data.frame(Nomi, Ordine)
print(table)

library("dplyr")

## arrange : serve per riordinare i dati in modo ascendente o discendente (segno meno davanti alla variabile)

#Arrange
table <- arrange(table, Nomi)
table

table <- arrange(table, Ordine)
table
