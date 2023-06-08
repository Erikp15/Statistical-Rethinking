library(rethinking)
d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)
d2 <- d[ d$age>17 , ]
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
d2$mid <- d2$married + 1
m1 <- quap(
alist(
	happiness ~ dnorm( mu , sigma ),
	mu <- a[mid] + bA*A,
	a[mid] ~ dnorm( 0 , 1 ),
	bA ~ dnorm( 0 , 2 ),
	sigma ~ dexp(1)
) , data=d2 )
m2 <- quap(
alist(
	happiness ~ dnorm( mu , sigma ),
	mu <- a + bA*A,
	a ~ dnorm( 0 , 1 ),
	bA ~ dnorm( 0 , 2 ),
	sigma ~ dexp(1)
) , data=d2 )
precis(m2)
precis(m1,depth=2)
PSIS(m1)
compare(m1,m2,func=PSIS)
compare(m1,m2,func=WAIC)