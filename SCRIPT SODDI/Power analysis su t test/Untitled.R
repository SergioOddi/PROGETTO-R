library(rpsychi)
library(pwr)


#Power analisi in post hoc test


# I dati dell'AEA nei Tg e Wt
m <- c(12.4, 12.5)
sd <- c(9.4, 7.3)
n <- c(12,11)

ind.t.test.second(m, sd, n,
                  unbiased = F, correct=F, sig.level = 0.05, digits = 3)

# La funzione mi informa che l'effect size (standardized.differenze, es:) è -0.011

# Per calcolare la potenza uso la funzione seguente, sostituendo a d il valore di es (-0.011)
pwr.t.test(n = 12, #Number of observations (per sample)
           d = -0.011, #Effect size (Cohen's d) - difference between the means divided by the pooled standard deviation
           sig.level = 0.05, 
           power = NULL, 
           type = c("two.sample"),
           alternative = c("two.sided"))

# CONCLUSIONE: l'esperimento ha un errore del secondo tipo molto elevato: 1 - 0.05 = 0.95
#Quindi, nel caso in cui sussistano differenze, con questo disegno sperimentale, 
#avrei il 95% di probabilità che l'esperimento non mi permetta di osservarle
# (c'è un'altissima probabilità di "falsi negativi")


# I dati dell'NO
m <- c(47.57, 23.16)
sd <- c(15.65, 10.68)
n <- c(7, 6)

ind.t.test.second(m, sd, n,
                  unbiased = F, correct=F, sig.level = 0.05, digits = 3)

# La funzione mi informa che l'effect size (standardized.differenze, es:) è 1.653

# Per calcolare la potenza uso la funzione seguente, sostituendo a d il valore di es (1.653)
pwr.t.test(n = 7, #Number of observations (per sample, qui mettiamo 7)
           d = 1.653, #Effect size (Cohen's d) - difference between the means divided by the pooled standard deviation
           sig.level = 0.05, 
           power = NULL, 
           type = c("two.sample"),
           alternative = c("two.sided"))

# CONCLUSIONE: l'esperimento ha un errore del secondo tipo di un liv accettabile: 1 - 0.81 = 0.19
#Quindi, nel caso in cui sussistano differenze, con questo disegno sperimentale, 
#avrei il 19% di probabilità che l'esperimento non mi permetta di osservarle
# (c'è un'accettabile probabilità di "falsi negativi")


