library(ProjectTemplate)
load.project()

## Ejemplo con tres variables.

tab.sc <- data.frame(tab.gencdimagen)

sexo <- as.numeric(tab.sc$Var1)
ciudad <- as.numeric(tab.sc$Var2)
eval.imagen <- as.numeric(tab.sc$Var3)
frec.1 <- tab.sc$Freq
n.celdas <- nrow(tab.sc)

model.file <- 'modelo_3_vars.bug'
jags.data <- c('sexo', 'ciudad', 'eval.imagen', 'frec.1', 'n.celdas')
jags.params <- c('beta.0', 'beta.s', 'beta.c', 'beta.sc', 'beta.se', 
		'beta.ce')

fit.poisson <- jags(model.file = model.file,
	data = jags.data, parameters.to.save = jags.params,
	n.chains = 2, n.burnin = 1000, n.thin = 10, DIC= TRUE,
	n.iter = 6000)

plot(fit.poisson)

