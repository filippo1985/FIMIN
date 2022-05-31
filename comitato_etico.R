set.seed(129)
library(simr)
library(tidyverse)
require(nlme)## for lme()
require(lme4)
require(multcomp)
require(car)
data<-readRDS("data.rds")

#Analisi preliminare da dati gia raccolti per un precedente studio
## Ogni paziente è campionato 2 volte quindi stimiamo un effetto random intercept per paziente 
mod.FI<-lmer(PTP~flow+(1|n_pz),data=data)
summary(mod.FI)
Anova(mod.FI)
res_mod<-residuals(mod.FI)
hist(res_mod)
shapiro.test(res_mod)

#simulazione del POWER  
modello.sim<-mod.FI
fixef(modello.sim)["flow"] <- 0.4
mod.fi<-extend(modello.sim,along = 'n_pz',n=60)
pc2<-powerCurve(mod.fi, nsim=100, along = 'n_pz', breaks = c(10,20,30))
pc2
