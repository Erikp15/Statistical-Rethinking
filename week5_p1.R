library(rethinking)
data(NWOGrants)
d <- NWOGrants
print(d)
dat <- list(
	G = ifelse(d$gender=="f",1,2),
	A = d$awards,
	N = d$applications
)
print(dat)
model <- ulam(
alist(
	A ~ dbinom(N,p),
	logit(p) <- a[G],
	a[G] ~ dnorm(0,1.5) 
),data=dat)
m <- extract.samples(model)
precis(model,depth=2)
plot(density(inv_logit(m$a[,1])-inv_logit(m$a[,2])))