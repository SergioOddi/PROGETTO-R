#### ANALISI delle Differenze tra valori sugli spermatozoi %



#### vitalità GF
t.test(x = vit_in_GF, y = vit_fin_GF, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)
### t = 4.66, df = 12, p-value = 0.00055
### mean of x mean of y 
#          63.857    45.857 
#diff: 18%


#### vitalità PF
t.test(x = vit_in_PF, y = vit_fin_PF, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)
### t = 7.93, df = 8, p-value = 0.000047
### mean of x mean of y 
#          39.8    7.4 
#diff: 32.4%



#### Motilità GF
t.test(x = mot_in_GF, y = mot_fin_GF, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)
### t = 4.87, df = 12, p-value = 0.00039
### mean of x mean of y 
#          67.857    51.429  
#diff: 16.4%

#### Motilità PF
t.test(x = mot_in_PF, y = mot_fin_PF, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)
### t = 4.66, df = 12, p-value = 0.00055
### mean of x mean of y 
#          45    13 
#diff: 32%



#Vitalità GF
vit_in_GF <- c(68, 61, 73, 60, 72, 50, 63)
vit_fin_GF <- c(45,
             40,
             58,
             50,
             45,
             43,
             40)

#Vitalità PF
vit_in_PF <- c(40,
               45,
               44,
               35,
               35)

vit_fin_PF <- c(7,
                8,
                0,
                2,
                20)

#Motilità GF
mot_in_GF <- c(70,
               65,
               75,
               70,
               75,
               55,
               65)

mot_fin_GF <- c(50,
                45,
                60,
                55,
                50,
                45,
                55)

#Motilità PF
mot_in_PF <- c(45,
               50,
               50,
               40,
               40)

mot_fin_PF <- c(10,
                15,
                5,
                5,
                30)


