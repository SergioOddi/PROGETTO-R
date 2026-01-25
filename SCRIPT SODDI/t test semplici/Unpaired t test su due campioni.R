# t TEST SEMPLICI su due campioni indipendenti (Unpaired t test)

####t-test con varianze uguali (i.e., var.equal = TRUE)
options(digits = 7)

t.test(data=dati, 
       Value ~ Treatment, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = TRUE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)


####t-test con varianze diverse (i.e., var.equal = TRUE) (with Welch's correction)
t.test(data=dati, 
       Value ~ Treatment, #a formula of the form Value ~ Treatment where Value is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
       alternative = c("two.sided"),
       paired = FALSE, #unpaired t test
       var.equal = FALSE, #se l'F-test mi dà un p value maggiore di 0.05
       conf.level = 0.95)
