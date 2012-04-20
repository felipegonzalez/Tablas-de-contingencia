library(ProjectTemplate)
library(FactoMineR)
library(Hmisc)
load.project()
data(tea)

#Segmentos de imagen
mca.tea <- MCA(tea[,25:36])

tea.2 <- data.frame(tea, mca.tea$ind$coord)
tea.2$gpo <- cut2(tea.2$Dim.1, g = 3)
tea.2$id <- 1:nrow(tea.2)
tab.seg <- ddply(tea.2, 'gpo', function(df){
  out.1 <- melt(df[,c(25:30,32:36,43)], id.vars='id')
  out.2 <- ddply(out.1, 'variable', function(df){
     data.frame(table(as.character(df$value)))
  })
  out.2 <- ddply(out.2, 'variable', transform, base=sum(Freq))
  out.2
})
tab.seg.2 <- subset(tab.seg, substr(Var1,1,2)!='No')
tab.seg.2$porcentaje <- round(100*tab.seg.2$Freq/tab.seg.2$base)
tab.seg.out <- cast(tab.seg.2, variable~gpo, value = 'porcentaje')
tab.seg.out[order(tab.seg.out[,2]),]
tea <- tea.2
tab.sc <- data.frame(table(tea$age_Q,tea$sex,
      tea$gpo))


## Análisis usual

edad <- as.numeric(tab.sc$Var1)
sexo <- as.numeric(tab.sc$Var2)
#ocupacion <- as.numeric(tab.sc$Var3)
gpo <- as.numeric(tab.sc$Var3)

frec.1 <- tab.sc$Freq
n.celdas <- nrow(tab.sc)

model.file <- './src/modelo_tea.bugs'
jags.data <- c('sexo', 'edad',  'gpo','frec.1', 'n.celdas')
jags.params <- c('beta.s', 'beta.e', 'beta.g', 'beta.sg', 
		'beta.eg', 'beta.se', 'sim.n')

fit.poisson <- jags(model.file = model.file,
	data = jags.data, parameters.to.save = jags.params,
	n.chains = 2, n.burnin = 1000, n.thin = 20, DIC= TRUE,
	n.iter = 15000)
fit.poisson
plot(fit.poisson)


#Resumen (simulación)

sims.n <- fit.poisson$BUGSoutput$sims.list$sim.n


sims.m <- melt(sims.n)
colnames(sims.m) <- c('sim.num', 'celda', 'Frec.sim')
tabla.2 <- data.frame(celda = 1:nrow(tab.sc), tab.sc)
sims.m.2 <- join(sims.m, tabla.2, by ='celda', type = 'left')


sims.porcentaje <- ddply(sims.m.2, c('Var1', 'Var2','sim.num'), 
	transform,
  	Porc.sim = Frec.sim/sum(Frec.sim),
  	Porc.obs = Freq/sum(Freq),
  	.progress = 'text')
cache('sims.porcentaje')
resumen.p <- ddply(sims.porcentaje, c('Var1', 'Var2','Var3'), summarise,
   sim.media = mean(Porc.sim, na.rm =TRUE), 
   superior = quantile(Porc.sim, 0.90, na.rm = TRUE),
   inferior = quantile(Porc.sim, 0.10, na.rm = TRUE),
   sim.sd = sd(Porc.sim, na.rm =TRUE),
   obs = mean(Porc.obs),
       .drop = FALSE)
resumen.p
resumen.p[resumen.p$Var1=='+60','Var1'] <- '60+'
ggplot(resumen.p, aes(x=Var3, y=100*sim.media,ymin=100*inferior,ymax=100*superior,
   colour=Var1, group=Var1)) + geom_point() + facet_grid(Var1~Var2) +
   geom_linerange(size=1.0) +
    geom_line() + 
   geom_point(aes(x=Var3, y=100*obs), size=3) + 
   col.ptos 
   
#Library L15L
library(L15LUtils)   
difsig(100*t(cast(resumen.p[,1:4], Var1+Var2 ~Var3)), c(65,27,26,43,25,15,39,22,23,15))
 
# ======================================================
# = Frecuencia/Tipo de te según segmento x edad x sexo =
# ======================================================







