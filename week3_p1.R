library(rethinking)
data(foxes)
d <- foxes
print(d)
dat <- list(
	A = d$area,
	F = d$avgfood
)
model <- quap(
	alist(
		F ~ dnorm(mu,sigma),
		mu <- a + b*A,
		a ~ dnorm(0,1),
		b ~ dnorm(0,1),
		sigma ~ dexp(1)
	),data=dat)
precis(model)
n <- 1e3
print(post)
model_pred <- link(model,data=list(A=1:5, F=0.4:1.2))
model_pred <- apply(model_pred,2,PI,0.99)
print(model_pred)
plot(d$area, d$avgfood, lwd=4, col=col.alpha(4,0.8))
shade(model_pred, 1:5, col=col.alpha(3,0.5))