#### Calcolo numerosità campionaria da un esperimento pilota con due gruppi

# Premessa: nella pratica scientifica, è importante considerare attentamente la numerosità campionaria prima di fare un qlq esperimento.
# Questo per una serie di ragioni:
# (i) evitare perdite di risorse di tempo e materiali
# (ii) per aspetti etici (numero di soggetti da arruolare: uomini e animali)
# (iii) per ragioni scientifiche (evitare errori statistici: falsi positivi e falsi negativi)

## Supponiamo di avere effettuato un esperimento pilota con una numerosità totale pari a 8
# gruppo controllo (1) Mean1 = 85, sd1 = 5, N1 = 4
# gruppo trattato (2) Mean1 = 95, sd2 = 5, N2 = 4

library(rmf)

# Analizziamo la significatività dell'esperimento pilota con un t test a due campioni
test.t2ci(media1= 90, ds1= 5, n1=4, media2= 85, ds2=5, n2=4)

# Risultato: il p value è 0.2, ossia > alpha (0.05), quindi la differenza tra le medie dei due gruppi non è stat. sig.
# Tuttavia, è legittimo chiedersi se la mancata significatività non possa essere dovuta a una scarsa potenza dell'esperimento.

### Punto da chiarire: ini questo caso sarebbe legittimo analizzare "post hoc" la potenza del test?
power.t.test(  n = 4, #numerosità (il numero di osservazioni per ciascun gruppo) si lascia "null", in caso si voglia computare la numerosità
               delta = 5, # il delta è la differenza tra le medie
               sd = 5,   # standard deviation campionaria
               sig.level = 0.05, # P value
               power = NULL, # potenza del test: 
               type = c("two.sample"), #tipo di t-test: one.sample, two.sample o paired
               alt="one.sided", #tipo di ipotesi: a una coda "one.sided" o a due code; "two.sided", "less", or "greater" to indicate a two-tailed, or one-tailed test. A two tailed test is the default.
               strict = TRUE) # opzione per decidere se includere entrambe le code nel calcolo della potenza, di default è FALSE.
# Risultato la potenza è 0.35, ossia la probabilità di non trovare risultati stat. sign. quando c'è una reale differenza è pari al 65% (forte probabilità di commettere errori falsi negativi)


# Qual è il risultato di qs studio? è quanto stavamo cercando. Il trattamento modifica la variabile?
# in questo caso l'effect size o parametro d di Cohen
# EFFECT SIZE: è la (differenza tra le due medie)/(ds comune)
# Per calcolare la ds comune (the pooled standard deviation - si può calcolare sul sito: https://www.psychometrica.de/effect_size.html)
# in questo caso = 90-85/5 = 1


# Lo studio pilota ci permette di avere una stima dell'effect size.
# Usando la power analisi a priori è possibile stimare la numerosità minima dei gruppi per raggiungere conclusioni scientificamente valide, 
# ossia con un livello desiderato di alpha e beta,
# quindi definendo i margini di errori falsi positivi (alfa) e falsi negativi (beta)


# Con power.t.test è possibile calcolare la numerosità campionaria per un dato effect size, per ottenere una data potenza statistica (in genere 0.8)

library(stats)
# t test di due campioni indipendenti (unpaired)
power.t.test(  n = NULL, #numerosità (il numero di osservazioni per ciascun gruppo) si lascia "null", in caso si voglia computare la numerosità
               delta = 5, # il delta è la differenza tra le medie
               sd = 5,   # standard deviation campionaria
               sig.level = 0.05, # P value
               power = 0.8, # potenza del test: 
               type = c("two.sample"), #tipo di t-test: one.sample, two.sample o paired
               alt="one.sided", #tipo di ipotesi: a una coda "one.sided" o a due code; "two.sided", "less", or "greater" to indicate a two-tailed, or one-tailed test. A two tailed test is the default.
               strict = TRUE) # opzione per decidere se includere entrambe le code nel calcolo della potenza, di default è FALSE.

# In modo alternativo, si può usare "pwr.t.test" del package "pwr"

library(pwr)

pwr.t.test(n = NULL,      #Number of observations (per sample)
           d = 1,         #Effect size (Cohen's d) - difference between the means divided by the “pooled" standard deviation
           sig.level = 0.05, 
           power = 0.8, 
           type = c("two.sample"), #tipo di t-test: one.sample, two.sample o paired
           alternative = c("greater")) #tipo di ipotesi: a una coda "one.sided" o a due code; "two.sided", "less", or "greater" to indicate a two-tailed, or one-tailed test. A two tailed test is the default.

# RISULTATO: Entrambi i test ci dicono che per avere una potenza di 0.8 e un liv. di sign. di 0.05 occorre avere campioni di 13-14 osservazioni.


power.t.test(  n = 18,                 # numerosità (il numero di osservazioni per ciascun gruppo) si lascia "null", in caso si voglia computare la numerosità
               delta = 52.6,           # il delta è la differenza tra le medie
               sd = 86.6,              # standard deviation campionaria
               sig.level = 0.05,       # P value
               power = NULL,           # potenza del test: 
               type = c("two.sample"), #tipo di t-test: one.sample, two.sample o paired
               alt="two.sided",        # tipo di ipotesi: a una coda "one.sided" o a due code; "two.sided", "less", or "greater" to indicate a two-tailed, or one-tailed test. A two tailed test is the default.
               strict = TRUE)          # opzione per decidere se includere entrambe le code nel calcolo della potenza, di default è FALSE.


pwr.t.test(n = 12,      #Number of observations (per sample)
           d = 0.79,         #Effect size (Cohen's d) - difference between the means divided by the “pooled" standard deviation
           sig.level = 0.05, 
           power = NULL, 
           type = c("two.sample"), #tipo di t-test: one.sample, two.sample o paired
           alternative = c("two.sided"))




