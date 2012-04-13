## Ejemplo de color de ojos y pelo
## Referencia: Kruschke. Doing Bayesian data analysis.

tabla.1 <- data.frame(HairEyeColor)
tabla.h <- subset(tabla.1, Sex == 'Female', select = -Sex)

#tabla.h$Freq <- round(tabla.h$Freq/3)

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

# ================
# = Simulaciones =
# ================

sims.n <- fit.poisson$BUGSoutput$sims.list$sim.n


sims.m <- melt(sims.n)
colnames(sims.m) <- c('celda', 'sim.num', 'Frec.sim')
tabla.2 <- data.frame(sim.num = 1:nrow(tabla.h), tabla.h)
sims.m.2 <- join(sims.m, tabla.2, by ='sim.num', type = 'left')

resumen.1 <- ddply(sims.m.2, c('Hair', 'Eye'), summarise,
   sim.media = mean(Frec.sim), sim.sd = sd(Frec.sim),
   obs= mean(Freq))