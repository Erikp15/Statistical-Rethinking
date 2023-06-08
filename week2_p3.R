library(rethinking)
data(Howell1)
d <- Howell1
d <- d[ d$age<13 , ]

dat <- list(
 W = d$weight,
 A = d$age,
 Abar = mean(d$age),
 S = d$male+1
)

model <- quap(
alist(
 W ~ dnorm(mu,sigma),
 mu <- a[S] + b[S]*A,
 a[S] ~ dnorm(60,10),
 b[S] ~ dlnorm(0,1),
 sigma ~ dunif(0,10)

 ), data=dat )

precis(model,depth=2)



plot( d$age , d$weight , lwd=2, col=ifelse(d$male==1,3,2) ,xlab="age" , ylab="weight" )

Ages <- 0:12
female_range <- link(model,data=data.frame(A=Ages,S=rep(1,13)))
shade( apply(female_range,2,PI,0.99) , Ages , col=col.alpha(4,0.5) )
male_range <- link(model,data=list(A=Ages,S=rep(2,13)))
shade( apply(male_range,2,PI,0.99) , Ages , col=col.alpha(3,0.5) )
