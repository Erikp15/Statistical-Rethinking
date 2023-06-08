library(rethinking)
data(UFClefties)
d <- UFClefties
print(d)
dat <- list(
	F1 = d$fighter1,
	L1 = d$fighter1.lefty+1,
	F2 = d$fighter2,
	L2 = d$fighter2.lefty+1,
	O = d$fighter1.win
)
print(dat)
model <- ulam(alist(
	O ~ dbinom(1,p),
	logit(p) <- a*(L1-L2),
	a ~ dnorm(0,1.5)
),data=dat,cores=4,threads=4)
precis(model,depth=3)
m <- extract.samples(model,1e3)
print(m)
dens(inv_logit(m$a))