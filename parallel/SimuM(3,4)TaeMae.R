#functions
pmatrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r = runif(m)
    r = r/sum(r) #generate row
    r[1:(m-1)] = round(r[1:(m-1)],3)
    r[m] = 1-sum(r[1:(m-1)])
    p[i,] = r
  }
  return(p)
}


l.vec.compute=function(k, cn.vec, m)
{
  k=k-1
  l.vec=rep(0, m-1)
  for(i in 1:(m-1))
  {
    aa=k%%cn.vec[i]
    bb=(k-aa)/cn.vec[i]
    l.vec[i]=bb
    k=aa
  }
  l.vec=l.vec+1
  return(l.vec)
}



f.simu.mae = function(n,m){
  pp = pmatrix(n,m)
  res0 = dpmd(pp)
  res1= dpmd(pp,method = "SIM-ALL",B=1000)
  mae = max(abs(res0-res1))
  return(mae)
}

f.simu.tae = function(n,m){
  pp = pmatrix(n,m)
  res0 = dpmd(pp)
  res1= dpmd(pp,method = "SIM-ALL",B=1000)
  tae = sum(abs(res0-res1))
  return(tae)
}
######################




library(doParallel)
library(foreach)
library(PMD)



# mae m=3



K= 100
n = c(1:100)
m = 3
N <- length(n)
results <- matrix(0,nrow = 1 ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE")

print("m=3, simulation, mae")
detectCores()
cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  mae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.simu.mae(n[i],m)
  }
  mae = sum(mae)/K
  results$`MAE` = mae
  print(results)
  
}

stopCluster(cl)



# m=4 simu mae

K= 10
n = c(1:10)
m = 4
N <- length(n)
results <- matrix(0,nrow = 1 ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE")

print("m=4, simulation, mae")
detectCores()
cl = makeCluster(4)
registerDoParallel(cl)

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  mae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.simu.mae(n[i],m)
  }
  mae = sum(mae)/K
  results$`MAE` = mae
  print(results)
  
}

stopCluster(cl)
