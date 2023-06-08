library(rethinking)
data(foxes)
d <- foxes
print(d)
dat <- list(
	G = (d$groupsize-mean(d$groupsize))/sd(d$groupsize),
	W = (d$weight-mean(d$weight))/sd(d$weight),
	F = (d$avgfood-mean(d$avgfood))/sd(d$avgfood)
)
print(dat)
model <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F + c*G,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		c ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
n <- 1e3
precis(model)
plot(dat$F, dat$W, lwd=4, col=col.alpha(4,0.8))
for (i in -2:3){
	model_pred <- link(model,data=list(W=-2:2, F=-2:2, G=i))
	model_pred <- apply(model_pred,2,PI,0.99)
	print(model_pred)
	shade(model_pred, -2:2, col=col.alpha(i+3,0.5))
}