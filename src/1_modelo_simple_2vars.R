## Ejemplo de color de ojos y pelo
## Referencia: Kruschke. Doing Bayesian data analysis.

tabla.1 <- data.frame(HairEyeColor)
tabla.h <- subset(tabla.1, Sex == 'Male', select = -Sex)

pelo <- as.numeric(tabla.h$Hair)
n.pelo <- length(levels(tabla.h$Hair))
ojos <- as.numeric(tabla.h$Eye)
n.ojos <- length(levels(tabla.h$Eye))
frec.1 <- tabla.h$Freq
n.celdas <- nrow(tabla.h)

model.file <- './src/modelo_simple.bugs'
jags.data <- c('pelo', 'ojos', 'frec.1', 'n.celdas', 'n.ojos', 'n.pelo')
jags.params <- c('beta.p', 'beta.o', 'beta.op', 'o.sigma', 'p.sigma', 'op.sigma', 'sim.n')

fit.poisson <- jags(model.file = model.file,
   data = jags.data, parameters.to.save = jags.params,
   n.chains = 3, n.burnin = 500, n.thin =1, DIC = FALSE,
   n.iter = 2000)


