library(rethinking)
data(Howell1)
data <- Howell1
dat <- data[data$age<13,]
print(dat)
#dat$height <- (dat$height-mean(dat$height))/sd(dat$height)
#dat$weight <- (dat$weight-mean(dat$weight))/sd(dat$weight)
print(dat)
linear_approximation <- ulam(
alist(
	weight ~ dnorm(mu,sigma),
	mu <- a + b*age,
	a ~ dnorm(0,1),
	b ~ dnorm(0,1),
	sigma ~ dexp(1)
),data=dat)
stancode(linear_approximation)
precis(linear_approximation)