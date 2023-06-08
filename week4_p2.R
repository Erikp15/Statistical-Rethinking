library(rethinking)
data(foxes)
d <- foxes
dat <- list(
	W = (d$weight-mean(d$weight))/sd(d$weight),
	F = (d$avgfood-mean(d$avgfood))/sd(d$avgfood),
	A = (d$area-mean(d$area))/sd(d$area),
	G = (d$groupsize-mean(d$groupsize))/sd(d$groupsize)
)
f <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
a <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*A,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
g <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*G,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
fa <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F + c*A,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		c ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
fg <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F + c*G,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		c ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
ag <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*A + c*G,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		c ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
fag <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F +  c*A + d*G,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		c ~ dnorm(0,0.5),
		d ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
PSIS(f)
PSIS(a)
PSIS(g)
PSIS(fa)
PSIS(fg)
PSIS(ag)
PSIS(fag)