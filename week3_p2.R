library(rethinking)
data(foxes)
d <- foxes
dat <- list(
	W = (d$weight-mean(d$weight))/sd(d$weight),
	F = (d$avgfood-mean(d$avgfood))/sd(d$avgfood)
)
model <- quap(
	alist(
		W ~ dnorm(mu,sigma),
		mu <- a + b*F,
		a ~ dnorm(0,0.5),
		b ~ dnorm(0,0.5),
		sigma ~ dunif(0,1)
	),data=dat)
precis(model)
n <- 1e3
print(model)
model_pred <- link(model,data=list(W=-2:2, F=-2:2))
model_pred <- apply(model_pred,2,PI,0.99)
print(model_pred)
plot(dat$F, dat$W, lwd=4, col=col.alpha(4,0.8))
shade(model_pred, -2:2, col=col.alpha(3,0.5))