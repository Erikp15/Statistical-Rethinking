library(rethinking)
library(dagitty)
data(bangladesh)
d <- bangladesh
dat <- list(
	A = d$age.centered,
	D = as.integer(d$district),
	K = d$living.children,
	C = d$use.contraception,
	U = d$urban
)

print(dat)
model <- ulam(alist(
	C ~ dbern(mu),
	logit(mu) <- a[D] + b*U,
	vector[61]:a ~ dnorm(abar,sigma),
	abar ~ dnorm(0,1.5),
	b ~ dnorm(0,1.5),
	sigma ~ dexp(1)
),data=dat)
precis(model,depth=3)
n <- 500
samp <- extract.samples(model)
post0 <- character(n)
for(i in 1:61){
	if(i!=54){
		for(j in 1:n){
			post0$a[,i] <- samp
		}
	}
}