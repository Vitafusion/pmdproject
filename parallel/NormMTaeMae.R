#functions
pmatrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r = runif(m)
    r = r/sum(r) #generate row
    r[1:(m-1)] = round(r[1:(m-1)],3)
    while(sum(r[1:(m-1)])>1){
        r = runif(m)
        r = r/sum(r)
        r[1:(m-1)] = round(r[1:(m-1)],3)
    }
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



f.norm = function(n,m){
  pp = pmatrix(n,m)
  res0 = dpmd(pp)
  res_normal=double(nnt)
  for(ii in 1:nnt)
  {
    idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
    if(nn-sum(idx-1)<0)
      res_normal[ii] = 0
    else
    {
      vec = c(idx-1,nn-sum(idx-1))
      res_normal[ii] = dpmd(pp, x = vec, method = 'NA')
    }
  }
  mae = max(abs(res0-res_normal))
  tae = sum(abs(res0-res_normal))
  return(c(mae,tae))
}
######################




#library(doParallel)
#library(foreach)
library(PMD)

print("NormMTaeMae")



# mae tae m=5


K = 50
n = c(1:50)
m = 5
N <- length(n)
results <- matrix(0,nrow = n ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

print("m=5,normal,mae tae")
detectCores()
cl = makeCluster(20)
registerDoParallel(cl)

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  tae = 0
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)

  # parallel here
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.norm(nn,m)
  }
  mae = sum(res[,1])/K
  results$`MAE` = mae
  results$`TAE` = sum(res[,2])/K
  print(results)
  
}





# mae tae m=6


K = 50
n = c(1:30)
m = 6
N <- length(n)
results <- matrix(0,nrow = n ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

print("m=6,normal,mae tae")

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  tae = 0
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)

  # parallel here
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.norm(n[i],m)
  }
  mae = sum(res[,1])/K
  results$`MAE` = mae
  results$`TAE` = sum(res[,2])/K
  print(results)
  
}



# mae tae m=7


K = 50
n = c(1:15)
m = 7
N <- length(n)
results <- matrix(0,nrow = n ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

print("m=7,normal,mae tae")

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  tae = 0
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)

  # parallel here
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.norm(n[i],m)
  }
  mae = sum(res[,1])/K
  results$`MAE` = mae
  results$`TAE` = sum(res[,2])/K
  print(results)
  
}


# mae tae m=8


K = 50
n = c(1:10)
m = 8
N <- length(n)
results <- matrix(0,nrow = n ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

print("m=8,normal,mae tae")

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  tae = 0
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)

  # parallel here
  res = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.norm(n[i],m)
  }
  mae = sum(res[,1])/K
  results$`MAE` = mae
  results$`TAE` = sum(res[,2])/K
  print(results)
  
}

stopCluster(cl)
