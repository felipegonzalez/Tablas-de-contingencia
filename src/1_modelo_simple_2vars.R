## Ejemplo de color de ojos y pelo
## Referencia: Kruschke. Doing Bayesian data analysis.

tabla.1 <- data.frame(HairEyeColor)
tabla.h <- subset(tabla.1, Sex == 'Female', select = -Sex)

#tabla.h$Freq <- round(tabla.h$Freq/2)

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
   n.chains = 2, n.burnin = 1000, n.thin =1, DIC = FALSE,
   n.iter = 2000)

plot(fit.poisson)
# ================
# = Simulaciones =
# ================

sims.n <- fit.poisson$BUGSoutput$sims.list$sim.n


sims.m <- melt(sims.n)
colnames(sims.m) <- c('sim.num', 'celda', 'Frec.sim')
tabla.2 <- data.frame(celda = 1:nrow(tabla.h), tabla.h)
sims.m.2 <- join(sims.m, tabla.2, by ='celda', type = 'left')

resumen.1 <- ddply(sims.m.2, c('Hair', 'Eye'), summarise,
   sim.media = mean(Frec.sim), sim.sd = sd(Frec.sim),
   obs= mean(Freq), .drop = FALSE)

sims.porcentaje <- ddply(sims.m.2, c('Hair', 'sim.num'), transform,
   Porc.sim = Frec.sim/sum(Frec.sim), Porc.obs = Freq/sum(Freq), .progress = 'text')

resumen.p <- ddply(sims.porcentaje, c('Hair', 'Eye'), summarise,
   sim.media = mean(Porc.sim, na.rm =TRUE), sim.sd = sd(Porc.sim, na.rm =TRUE),
   obs= mean(Porc.obs), .drop = FALSE)
resumen.p

ggplot(resumen.p, aes(x=Hair, y=sim.media, ymin = sim.media - 2*sim.sd,
   ymax = sim.media + 2*sim.sd,
   colour=Eye, group=Eye)) + geom_point() +
   geom_linerange() + geom_line() + geom_point(aes(x=Hair, y=obs), size=3)