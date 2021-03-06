library(ProjectTemplate)
load.project()

## Ejemplo con tres variables.

tab.sc <- data.frame(tab.gencdimagen)

## Análisis usual

tab.sc.2 <- ddply(tab.sc, c('Var1', 'Var2'), transform, prop=Freq/sum(Freq),
   base = sum(Freq))
tab.usual <- cast(tab.sc.2, Var3~Var2+Var1, value='prop')
tab.base <- cast(tab.sc.2, Var3~Var2+Var1, value='base')
difsig(100*as.matrix(tab.usual[,-1]), as.numeric(tab.base[1,-1]))
ggplot(tab.sc.2, aes(x=Var3, y=prop, colour=Var2, group=Var2)) + geom_point() + facet_wrap(~Var1)+
   geom_line() + ylim(c(0,1))

sexo <- as.numeric(tab.sc$Var1)
ciudad <- as.numeric(tab.sc$Var2)
eval.imagen <- as.numeric(tab.sc$Var3)
frec.1 <- tab.sc$Freq
n.celdas <- nrow(tab.sc)

model.file <- './src/modelo_3_vars.bugs'
jags.data <- c('sexo', 'ciudad', 'eval.imagen', 'frec.1', 'n.celdas')
jags.params <- c('beta.0', 'beta.s', 'beta.c', 'beta.e', 'beta.sc', 'beta.se', 
		'beta.ce', 'sim.n', 'beta.sce')

fit.poisson <- jags(model.file = model.file,
	data = jags.data, parameters.to.save = jags.params,
	n.chains = 2, n.burnin = 1000, n.thin = 20, DIC= TRUE,
	n.iter = 25000)

plot(fit.poisson)


#Resumen (simulación)

sims.n <- fit.poisson$BUGSoutput$sims.list$sim.n


sims.m <- melt(sims.n)
colnames(sims.m) <- c('sim.num', 'celda', 'Frec.sim')
tabla.2 <- data.frame(celda = 1:nrow(tab.sc), tab.sc)
sims.m.2 <- join(sims.m, tabla.2, by ='celda', type = 'left')


sims.porcentaje <- ddply(sims.m.2, c('Var1', 'Var2', 'sim.num'), 
	transform,
  	Porc.sim = Frec.sim/sum(Frec.sim), Porc.obs = Freq/sum(Freq), 
  	.progress = 'text')

resumen.p <- ddply(sims.porcentaje, c('Var1', 'Var2', 'Var3'), summarise,
   sim.media = mean(Porc.sim, na.rm =TRUE), 
   superior = quantile(Porc.sim, 0.975, na.rm = TRUE),
   inferior = quantile(Porc.sim, 0.025, na.rm = TRUE),
   sim.sd = sd(Porc.sim, na.rm =TRUE),
   obs= mean(Porc.obs), .drop = FALSE)
resumen.p

ggplot(resumen.p, aes(x=Var3, y=sim.media, ymin = inferior,
   ymax = superior,
   colour=Var2, group=Var2)) + geom_point() + facet_wrap(~Var1) +
   geom_linerange(size=1.0) + geom_line() + 
  # geom_point(aes(x=Var3, y=obs), size=3) + 
   col.ptos 
   
 

##############
## Ciudad y edad
#############

tab.sc <- data.frame(tab.genedadimagen)

sexo <- as.numeric(tab.sc$Var1)
edad <- as.numeric(tab.sc$Var2)
eval.imagen <- as.numeric(tab.sc$Var3)
frec.1 <- tab.sc$Freq
n.celdas <- nrow(tab.sc)

model.file <- './src/modelo_3_vars_2.bugs'
jags.data <- c('sexo', 'edad', 'eval.imagen', 'frec.1', 'n.celdas')
jags.params <- c('beta.0', 'beta.s', 'beta.c', 'beta.e', 'beta.sc', 'beta.se', 
		'beta.ce', 'sim.n')

fit.poisson <- jags(model.file = model.file,
	data = jags.data, parameters.to.save = jags.params,
	n.chains = 2, n.burnin = 2000, n.thin = 20, DIC= FALSE,
	n.iter = 16000)

plot(fit.poisson)



