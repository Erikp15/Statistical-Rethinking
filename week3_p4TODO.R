library(rethinking)
n <- 10
a <- 1
b <- 0.5
s <- 1
d <- list(
	A=rnorm(n,0,s)
	U=rnorm(n,0,s),
	S=rnorm(n,a*U+b*A,s),
	X=rnorm(n,
)
print(d)
