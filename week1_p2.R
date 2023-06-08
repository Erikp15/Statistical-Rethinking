prior <- c(rep(0,500),rep(1,500))
probabilities <- seq(from=0,to=1,length.out=1000)
res <- dbinom(4,size=6,prob=probabilities)
post <- res*prior
posterior <- post/sum(post)
print(sum(posterior))
plot(probabilities, posterior, type="l" , xlab="proportion water", ylab="proportion density")
