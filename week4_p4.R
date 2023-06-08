library(rethinking)
data(Dinosaurs)
d <- Dinosaurs
type <- 1
d <- d[d$sp_id==type,]
print(d)
dat <- list(
	A = d$age,
	M = d$mass/max(d$mass),
	S = d$sp_id
)
m1 <- ulam(
alist(
	M ~ dnorm(mu,sigma),
	mu <- a + b*A,
	a ~ dnorm(0,1),
	b ~ dnorm(0,1),
	sigma ~ exponential(1)
),data=dat, chains=4, log_lik=TRUE)
n <- 1e4
plot(dat$A,dat$M,xlim=c(0,16),lwd=2,col=col.alpha(3,0.7))
mu <- link(m1,data=list(A=0:16))
print(mu)
b <- apply(mu,2,PI)
#print(b[1,])
plot(0:16,b[1,],col=col.alpha(4,0.7),lwd=2)