library(rethinking)
data(reedfrogs)
d <- reedfrogs
print(d)

dat <- list(
	D = d$density,
	P = ifelse(d$pred=="no",1L,2L),
	S = ifelse(d$size=="small",1L,2L),
	L = d$surv,
	T = 1:nrow(d)
)
print(dat)
model <- ulam( 
alist(
	L ~ dbinom(D,p),
	logit(p) <- a[P,S] + b[T] + c[P,D],
	matrix[P,S]:a ~ dnorm(0,1),
	b[T] ~ dnorm(0,sigma),
	sigma ~ exponential(1)
),data=dat,chains=4,cores=4)

precis(model,depth=3)