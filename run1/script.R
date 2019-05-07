library(ape)
mydata <- read.csv ("Final4.csv")
mytree2 <- read.tree("mytree.tree") 
states <- mydata$Pol2
names(states) <- mydata$Nombres
phy<-mytree2
library(diversitree)

sampling.f <- 0.6
lik <- make.bisse(phy, states, sampling.f=sampling.f)
pars <- c(0.1, 0.2, 0.03, 0.03, 0.01, 0.01)
lik(pars) 
p <- starting.point.bisse(phy)
p
fit <- find.mle(lik, p)
fit$lnLik
round(coef(fit), 3)

prior <- make.prior.exponential(1 / (2 * (p[1] - p[3])))
set.seed(1)
tmp <- mcmc(lik, fit$par, nsteps=1000, prior=prior,lower=0, w=rep(1, 6), print.every=0)
w <- diff(sapply(tmp[2:7], range))
w
samples <- mcmc(lik, fit$par, nsteps=1000000, w=w, lower=0, prior=prior,print.every=1000)

save.image(file="samples.RData")
