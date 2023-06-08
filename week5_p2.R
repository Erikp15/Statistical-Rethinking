library(rethinking)
data(NWOGrants)
d <- NWOGrants
print(d)
dat <- list(
	G = ifelse(d$gender=="f",1,2),
	A = d$awards,
	N = d$applications,
	D = as.integer(d$discipline)
)
print(dat)
model <- ulam(
alist(
	A ~ dbinom(N,p),
	logit(p) <- a[G,D],
	matrix[G,D]:a ~ dnorm(0,1.5)
),data=dat,cores=4,threads=4)

stancode(model)
#m <- extract.samples(model)
precis(model,depth=3)
total_apps <- sum(dat$N)

apps_for_d <- sapply( 1:9 , function(i) sum(dat$N[dat$D==i]) )

sim_for_women <- link(model,data=list(
	D = rep(1:9,times=apps_for_d),
	G = rep(1,total_apps)
))
sim_for_men <- link(model,data=list(
	D = rep(1:9,times=apps_for_d),
	G = rep(2,total_apps)
))

dens(sim_for_women - sim_for_men)
