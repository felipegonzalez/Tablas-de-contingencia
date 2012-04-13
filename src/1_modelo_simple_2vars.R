library(ProjectTemplate)
load.project()

## Ejemplo de color de ojos y pelo
## Referencia: Kruschke. Doing Bayesian data analysis.

tabla.1 <- data.frame(HairEyeColor)
tabla.h <- subset(tabla.1, Sex == 'Female', select = -Sex)
tabla.h$num <- 1:nrow(tabla.h)
sims.personas <- data.frame(table(sample(1:16, 100, replace = TRUE, prob=tabla.h$Freq)))
names(sims.personas) <- c('num', 'Freq.2')
tabla.h.2 <- join(tabla.h, sims.personas, by ='num')
tabla.h.2$Freq.2[is.na(tabla.h.2$Freq.2)] <- 0
tabla.h.2
tabla.h.2$Freq <- NULL
tabla.h.2$Freq <- tabla.h.2$Freq.2
tabla.h <- tabla.h.2
#tabla.h$Freq <- round(tabla.h$Freq/4)


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
   n.chains = 2, n.burnin = 1000, n.thin =10, DIC = FALSE,
   n.iter = 8000)

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

## Pésima elección de colores con las etiquetas!
ggplot(resumen.p, aes(x=Hair, y=sim.media, ymin = sim.media - 2*sim.sd,
   ymax = sim.media + 2*sim.sd,
   colour=Eye, group=Eye)) + geom_point() +
   geom_linerange(size=1.5) + geom_line() + geom_point(aes(x=Hair, y=obs), size=3) + 
   col.ptos
   

# =======================
# = Perfiles            =
# =======================   
sims.perfiles <- ddply(sims.porcentaje, c('sim.num', 'Eye'), transform,
   perfil.col = 100*Porc.sim/mean(Porc.sim), 
   obs.perfil = 100*Porc.obs/mean(Porc.obs),
   .progress ='text')

resumen.perf <-   ddply(sims.perfiles, c('Hair', 'Eye'), summarise,
   sim.media = mean(perfil.col, na.rm =TRUE), sim.inf = quantile(perfil.col, 0.05, na.rm=TRUE),
   sim.sup = quantile(perfil.col, 0.95, na.rm = TRUE),
   perfil.obs= mean(obs.perfil), .drop = FALSE) 


ggplot(resumen.perf, aes(x=Hair, y=sim.media, ymin = sim.inf,
   ymax = sim.sup)) + geom_hline(yintercept = 100, col = 'gray') +
   geom_linerange(size=2, alpha=0.5, col ='salmon') + 
   geom_point(size=2) +
   geom_line() + facet_wrap(~Eye) +
   geom_point(aes(x=Hair, y=perfil.obs), size=3)

ggplot(resumen.perf, aes(x=Eye, y=sim.media, ymin = sim.inf,
   ymax = sim.sup, group = Hair)) + 
    geom_linerange(size=2, alpha=0.7, colour='salmon') +
   geom_hline(yintercept = 100, col = 'gray') + geom_point(size=2) +
   geom_line(colour='salmon') + facet_wrap(~Hair)  

