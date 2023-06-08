prior <- rep(1,1000)
probabilities <- seq(from=0,to=1,length.out=1000)
res <- dbinom(1,size=900,prob=0.0009)
print(res)
post <- res*prior
posterior <- post/sum(post)
plot(probabilities, posterior, type="l" , xlab="proportion water", ylab="proportion density")
