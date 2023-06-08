library(rethinking)
data(bangladesh)
d <- bangladesh
print(d)
dat <- list(
	A = d$age.centered,
	D = d$district,
	C = d$living.children,
	U = d$use.contraception
)
#actual prob
for (i in 1:61){
     	print(sum(d[d$district==i,]$use.contraception)/nrow(d[d$district==i,]))
}
model <- ulam(alist(
	U ~ dbern(p),
	logit(p) <- a[D],
	vector[61]:a ~ dnorm(abar,sigma),
	abar ~ dnorm(0,1.5),
	sigma ~ dexp(1)
),data=dat)
plot(inv_logit(precis(model,depth=3)))