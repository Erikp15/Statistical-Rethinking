library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
d <- na.omit(d)
dat <- list(
	Y = d$year,
	D = d$doy,
	T = d$temp
)
d1 <- quap(
alist(
	D ~ dnorm(mu,sigma),
	mu <- a + b*T + c*Y,
	a ~ dnorm(0,10),
	b ~ dnorm(0,10),
	c ~ dnorm(0,10),
	sigma ~ dexp(1)
),data=dat)
PSIS(d1)
n <- 1e5
post <- extract.samples(d1,n=n)
samp <- rnorm(n,mean(post$a)+mean(post$b)*9+mean(post$c)*2050,mean(post$sigma))
dens(samp)
