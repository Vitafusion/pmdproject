# DFT-CF time efficiency study


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


pmd.effi <- function(n,m){
  pp <- p.matrix(n,m)
  t <- system.time(dpmd(pp))
  return(c(n,m,t[3]))
}


library(foreach)
library(doParallel)
library(PoissonMultinomial)
print('efficiency study of m=6, K=1000')


detectCores()
cl = makeCluster(100)
registerDoParallel(cl)

#m=2
K <- 1000
n <- c(1:3)
N <- length(n)
m <- 6

results <- matrix(0, nrow = N, ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c('n', 'm', 'time')

for (i in n) {
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PoissonMultinomial',.combine = 'rbind') %dopar% {
    pmd.effi(10*i,m)
  }
  results$`n`[i] <- 10*i
  results$`m`[i] <- m
  results$`time`[i] <- mean(res[,3])
  print(results[i,], sep='\t')
}

stopCluster(cl)
write.table(results,'effi_m6.txt', sep = '\t')