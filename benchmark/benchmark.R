## benchmark of poibin and poissonmulti
p.matrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r = runif(m)
    r = r/sum(r) #generate row
    r[1:(m-1)] = round(r[1:(m-1)],3)
    while(sum(r[1:(m-1)])>1){
      r = runif(m)
      r = r/sum(r) #generate row
      r[1:(m-1)] = round(r[1:(m-1)],3)
    }
    r[m] = 1-sum(r[1:(m-1)])
    p[i,] = r
  }
  return(p)
}


library(poibin)
library(PoissonMultinomial)

m <- 2
n <- 1:1000
N <- length(n)
K <- 1000
bench.res <- c()



for(i in 1:N){
  t1 <- 0
  t2 <- 0
  for(k in 1:K){
    pp = p.matrix(10*i,m)
    mm=ncol(pp) # m categories
    nn=nrow(pp) # n people
    t1 <- t1 + system.time(dpmd(pp))
    t2 <- t2 + system.time(dpoibin(0:nn, pp[,1]))
  }
  t1 <- t1/K
  t2 <- t2/K
  print(c(10*i,t1,t2), sep='\t')
  bench.res <- rbind(bench.res, c(10*i,t1,t2))
}

write.table(bench.res, 'benchmark.txt', sep='\t')