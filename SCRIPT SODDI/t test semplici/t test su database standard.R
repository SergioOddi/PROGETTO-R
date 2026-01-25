###### t test su un database classico

# ESEMPIO:
# A tibble: 12 x 7
#ID         Genotype Group   `AEA (picomol/mL)` `2-AG (picomol/mL)` `PEA (nanomol/mL)` `OEA (picomol/mL)`
#<chr>      <chr>    <chr>                <dbl>               <dbl>              <dbl>              <dbl>
#Tg2576      Tg       PEA          36.7               18.4                1.76               0.11
#Tg2576      Tg       PEA          27.8                8.07               0.69               0.16
#WT          Wt       PEA          74.2               25.6                0.54               0.15
#WT          Wt       PEA          26.5               10.3                0.65               0.1 


#### Carico il database e lo ribattezzo dati_completi
dati_completi <- Campioni_sangue_Topi_FINALIZZATA_2nd_tornata

##### rimuovo l'outlier
dati_completi = dati_completi[-27, ]


##### Se serve rinominare le colonne
names(dati_completi) <- c("ID","Tornata", "Sex", "Genotype", "Group", "AEA", "AG", "PEA", "OEA")

# Eventualmente rimuovo uno o più livelli per avere un certo numero di gruppi
dati_st <- subset(dati_completi, Tornata != "Prima")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Tornata è "Prima"
dati_st

# Eventualmente rimuovo uno o più livelli per avere un certo numero di gruppi
dati_Tg <- subset(dati_st, Genotype != "Wt")  # In questo caso rimuovo dal database le righe il cui fattore nella colonna Genotype è "Wt"
dati_Tg

# Eventualmente rimuovo uno o più livelli per avere un certo numero di gruppi
dati_Wt <- subset(dati_st, Genotype != "Tg")
dati_Wt

### t test senza dividere i topi in Tg e Wt
t.test(data=dati_st,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       PEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Tg
t.test(data=dati_Tg,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       PEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Wt
t.test(data=dati_Wt,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       PEA ~ Group, # a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)


### t test senza dividere i topi in Tg e Wt
t.test(data=dati_st,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Tg
t.test(data=dati_Tg,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Wt
t.test(data=dati_Wt,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AEA ~ Group, # a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test senza dividere i topi in Tg e Wt
t.test(data=dati_st,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       OEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Tg
t.test(data=dati_Tg,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       OEA ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Wt
t.test(data=dati_Wt,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       OEA ~ Group, # a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)


t.test(data=dati_st,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AG ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Tg
t.test(data=dati_Tg,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AG ~ Group, #a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

### t test sui Wt
t.test(data=dati_Wt,    ### data vuole il nome del database su cui prendere i valori (nella forma standard)
       AG ~ Group, # a formula of the form PEA ~ Group where PEA is a numeric variable giving the data values and Group a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)

