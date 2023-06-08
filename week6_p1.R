library(rethinking)
n <- 1e4

# 0.1
sigma <- rexp(n,0.1)
abar <- rnorm(n,0,1)
a <- rnorm(n,abar,sigma)
dens(inv_logit(a))

# 1
sigma <- rexp(n,1)
abar <- rnorm(n,0,1)
a <- rnorm(n,abar,sigma)
dens(inv_logit(a))

# sigma=10
sigma <- rexp(n,10)
abar <- rnorm(n,0,1)
a <- rnorm(n,abar,sigma)
dens(inv_logit(a))