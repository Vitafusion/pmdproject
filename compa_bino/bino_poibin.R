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
n <- 1:300
N <- length(n)
K <- 1000
binom.res <- matrix(0, nrow = N, ncol = 3)
binom.res <- as.data.frame(binom.res)
colnames(binom.res) <- c('n','mae','tae')
temp <- binom.res[1,2:3]


for(i in 1:N){
  for(k in 1:K){
    p.bino <- p.matrix(n=1, m=2)
    pp <- matrix(rep(p.bino,10*i),nrow=10*i,byrow=T)
    mm <- ncol(pp) # m categories
    nn <- nrow(pp) # n people
    res0 <- dpmd(pp)
    res1 <- dbinom(0:nn, nn, p.bino[1])
    temp[1] <- max(abs(res0-res1))
    temp[2] <- sum(abs(res0-res1))
    binom.res[i,2:3] <- temp + binom.res[i,2:3]
  }
  binom.res$`n`[i] <- nn 
  binom.res[i,2:3] <- binom.res[i,2:3]/K
}

binom.res
write.table(binom.res, 'binom.txt', sep = '\t')

#plot(x=binom.res$`n`, y=binom.res$`mae`, type='l',
#     main="Binomial vs 'DFT-CF'", xlab="n", ylab="mae")

#plot(x=binom.res$`n`, y=binom.res$`tae`, type='l',
#     main="Binomial vs 'DFT-CF'", xlab="n", ylab="tae")


m <- 2
n <- 1:300
N <- length(n)
K <- 1000
pbino.res <- matrix(0, nrow = N, ncol = 3)
pbino.res <- as.data.frame(pbino.res)
colnames(pbino.res) <- c('n','mae','tae')
temp <- pbino.res[1,2:3]


for(i in 1:N){
  for(k in 1:K){
    pp = p.matrix(10*i,m)
    mm=ncol(pp) # m categories
    nn=nrow(pp) # n people
    res0 = dpmd(pp)
    res1 = dpoibin(0:nn, pp[,1])
    temp[1] <- max(abs(res0-res1))
    temp[2] <- sum(abs(res0-res1))
    pbino.res[i,2:3] <- temp + pbino.res[i,2:3]
  }
  pbino.res$`n`[i] <- 10*i
  pbino.res[i,2:3] <- pbino.res[i,2:3]/K
}

write.table(pbino.res, 'pbino.txt', sep = '\t')

#plot(x=pbino.res$`n`, y=pbino.res$`mae`, type='l',
#     main="Poisson-Binomial vs 'DFT-CF'", xlab="n", ylab="mae")

#plot(x=pbino.res$`n`, y=pbino.res$`tae`, type='l',
#     main="Poisson-Binomial vs 'DFT-CF'", xlab="n", ylab="tae")