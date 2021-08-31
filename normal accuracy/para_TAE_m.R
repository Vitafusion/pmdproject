#TAE
#normal method normal, m = 3
#n = 100 here

source("/home/zhengzhi/functions/myfunctions.R")
dyn.load("/home/zhengzhi/functions/functions.so")


#dyn.load('functions.so')
#source('myfunctions.R')
print('m=3')




normal.simu.avg = function(K,n,m){
temp_dat <- matrix(0,nrow = K ,ncol = 2)
  temp_dat <- as.data.frame(temp_dat)
  colnames(temp_dat) <- c("MAE","TAE")
  for (k in 1:K) {
    pp = pmatrix(n,m) 
    mm = m
    nn = n
    tae = 0
    mae = 0
    mse = 0
    nn.vec=rep(nn+1, mm-1)
    l.vec=rep(0, mm-1)
    cn.vec=cumprod(nn.vec)
    cn.vec=c(1, cn.vec[-(mm-1)])
    cn.vec=cn.vec[length(cn.vec):1]
    cn.vec=as.integer(cn.vec)
    nnt=prod(nn.vec)
    res_normal=double(nnt)
    res0 = pmd(pp)
    for(ii in 1:nnt)
    {
      idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
      if(nn-sum(idx-1)<0)
        res_normal[ii] = 0
      else
      {  
        vec = c(idx-1,nn-sum(idx-1))
        res_normal[ii] = pmd(pp,method = 'NA',vec)
      }
    }
    ac = abs(res0 - res_normal)
    tae = sum(ac)
    mae = max(ac)
    temp_dat$MAE[k] = mae
    temp_dat$TAE[k] = tae
  }
  res = c(n,formatC(mean(temp_dat$MAE),format = 'e',digits = 5),formatC(mean(temp_dat$TAE), format = "e", digits = 5))
  return(res)
}


normal.simu.avg(10,10,3)


#sfSource("/home/zhengzhi/functions/myfunctions.R")
#sfClusterEval(dyn.load("/home/zhengzhi/functions/functions.so"))
n = c(1:10)
m = 3
N <- length(n)
results <- matrix(0,nrow = N ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

for(i in 1:N){
  #results[i,]=normal.simu.avg(10,n[i],3)
 normal.simu.avg(10,n[i],3)
}

library(Rcpp)
library(parallel)
library(foreach)
library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
getDoParRegistered()


foreach(i = 1:N) %dopar% {
  print(normal.simu.avg(10,n[i],3))
}