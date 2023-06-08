
library(rethinking)
data(Howell1)
data <- Howell1
over_18 <- data[data$age>=18,]
print(over_18)
vals <- seq(-100,100,0.01)
data_weights <- over_18$weight
data_heights <- over_18$height
average_height <- mean(over_18$height)
dat <- list(W=data_weights, H=data_heights, Hbar=average_height)
over_18$height.c <- over_18$height-mean(over_18$height)
linear_approximation <- map(
alist(
	weight ~ dnorm(mu,sigma),
	mu <- a + b*height,
	a ~ dnorm(0,20),
	b ~ dnorm(0,20),
	sigma ~ dunif(0,20)
),data=over_18)
post <- extract.samples(linear_approximation)
#print(post)
plot(weight ~ height,data=over_18)
abline(a=coef(linear_approximation)["a"], b=coef(linear_approximation)["b"])
for( i in 1:20 )
	abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )
ans_140 <- post$a + post$b * 140
ans_160 <- post$a + post$b * 160
ans_175 <- post$a + post$b * 175
