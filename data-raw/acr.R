library(nimble)

test <- nimbleCode({


model{
  for(i in 1:ns) {
    r[i] ~ dbin(p[i],n[i])
    logit(p[i]) <- mu[i]
    mu[i] ~ dnorm(m, tau.m)
  }
}

mu.new ~ dnorm(m, tau.m)

m ~ dnorm(0, hyerVarInNormMean)
sd.m ~ dunif(0, hyperSDInUnif)
tau.m <- pow(sd.m, -2)

})
