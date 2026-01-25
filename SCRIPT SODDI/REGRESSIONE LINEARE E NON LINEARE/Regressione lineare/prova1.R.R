library(ggplot2)

df <- data.frame(x = c(1:100))

df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)

m <- lm(y ~ x, data = df)

p <- ggplot(data = df, aes(x = x, y = y)) +
  
  geom_smooth(method = "lm", formula = y ~ x) +
  
  geom_point()

p



eq <- substitute(italic(y)==a+b%.%italic(x)*","~~italic(r)^2~"="~r2,
                 
                 list(        a = format(coef(m)[1], digits = 4),
                              
                              b = format(coef(m)[2], digits = 4),
                              
                              r2 = format(summary(m)$r.squared, digits = 3)))



dftext <- data.frame(x = 70, y = 50, eq = as.character(as.expression(eq)))

p + geom_text(aes(label = eq), data = dftext, parse = TRUE)

