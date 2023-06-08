library(rethinking)
data(reedfrogs)
d <- reedfrogs
print(d)

dat <- list(
	D = d$density,
	P = as.integer(d$pred),
	S = as.integer(d$size),
	L = d$surv,
	T = 1:nrow(d)
)

dat$Do=standardize(dat$D)

print(dat)
model <- ulam( 
alist(
	L ~ dbinom(D,p),
	logit(p) <- a[P,S] + b[T] + c[P]*Do,
	matrix[P,S]:a ~ dnorm(0,1),
	b[T] ~ dnorm(0,sigma),
	c[P] ~ dnorm(0,1),
	sigma ~ exponential(1)
),data=dat,chains=4,cores=4)
post <- extract.samples(model)
precis(model,depth=3)

n <- 20
D <- rep(c(10,35,10,35),n)
Do <- standardize(D)
P <- rep(rep(2,4),n)
S <- rep(c(1,1,2,2),n)

bT <- replicate(length(D),rnorm(2000,0,post$sigma))

ptrue <- sapply(1:length(D),
	function(i) inv_logit(post$a[,P[i],S[i]]+bT[,i]+post$c[,P[i]]*post$c*Do[i]))
P <- rep(rep(1,4),n)
pfalse <- sapply(1:length(D),
	function(i) inv_logit(post$a[,P[i],S[i]]+bT[,i]+post$c[,P[i]]*post$c*Do[i]))

plot(density(pfalse-ptrue),xlim=0:1)
