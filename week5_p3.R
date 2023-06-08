library(rethinking)
data(NWOGrants)
d <- NWOGrants
print(d)
dat <- list(
	G = ifelse(d$gender=="f",1,2),
	A = d$awards,
	N = d$applications,
	D = as.integer(d$discipline)
)
print(dat)
total_women <- sum(dat$N[dat$G==1])
total_men <- sum(dat$N[dat$G==2])
print(total_women)
prop_total_f <- dat$N[dat$G==1]/total_women
prop_total_m <- dat$N[dat$G==2]/total_men
print(prop_total_m)

n_apps <- xtabs( dat$N ~ dat$D )
n_awards <- xtabs( dat$A ~ dat$D )
p_award <- n_awards / n_apps

post <- extract.samples(m2)
pF <- apply( inv_logit(post$a[,1,]) , 2 , mean )
pM <- apply( inv_logit(post$a[,2,]) , 2 , mean )

plot( prop_total_f , prop_total_m , lwd=3 , col=ifelse(prop_total_f>prop_total_m,2,4)
,pch=ifelse(pF>pM,"F","M") )
abline(a=0,b=1, lty=3)

identify( prop_total_f , prop_total_m , round(p_award,2) )