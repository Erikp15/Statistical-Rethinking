library(rethinking)
library(dagitty)
data(bangladesh)
d <- bangladesh
dat <- list(
	A = d$age.centered,
	D = d$district,
	K = d$living.children,
	C = d$use.contraception,
	U = d$urban
)
DAG <- dagitty("dag{
	A -> K	
	A -> C	
	D -> K
	D -> C
	D -> U
	C -> K
	U -> K
	U -> C
}")
drawdag(DAG)