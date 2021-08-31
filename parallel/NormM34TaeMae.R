#functions
pmatrix <- function(n,m){
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


avg.mae.norm = function(kk, n, m){
    mm = m
    nn = n
    mae = 0
    nn.vec=rep(nn+1, mm-1)
    l.vec=rep(0, mm-1)
    cn.vec=cumprod(nn.vec)
    cn.vec=c(1, cn.vec[-(mm-1)])
    cn.vec=cn.vec[length(cn.vec):1]
    cn.vec=as.integer(cn.vec)
    nnt=prod(nn.vec)
    for(i in 1:kk){
      pp = pmatrix(n,m)
      res0 = pmd(pp)
      res_normal=double(nnt)
      for(ii in 1:nnt)
      {
        idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
        #print(idx)
        if(nn-sum(idx-1)<0)
          res_normal[ii] = 0
        else
        {
          vec = c(idx-1,nn-sum(idx-1))
          res_normal[ii] = pmd(pp, x = vec, method = 'NA')
        }
      }
      ac = abs(res0 - res_normal)
      #if(res1 < ac)
      mae = max(ac) + mae
    }
    mae = mae/kk
    return(mae)
}


avg.mae.norm = function(kk, n, m){
    mm = m
    nn = n
    mae = 0
    nn.vec=rep(nn+1, mm-1)
    l.vec=rep(0, mm-1)
    cn.vec=cumprod(nn.vec)
    cn.vec=c(1, cn.vec[-(mm-1)])
    cn.vec=cn.vec[length(cn.vec):1]
    cn.vec=as.integer(cn.vec)
    nnt=prod(nn.vec)
    for(i in 1:kk){
      pp = pmatrix(n,m)
      res0 = dpmd(pp)
      res_normal=double(nnt)
      for(ii in 1:nnt)
      {
        idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
        #print(idx)
        if(nn-sum(idx-1)<0)
          res_normal[ii] = 0
        else
        {
          vec = c(idx-1,nn-sum(idx-1))
          res_normal[ii] = dpmd(pp, x = vec, method = 'NA')
        }
      }
      ac = abs(res0 - res_normal)
      #if(res1 < ac)
      mae = max(ac) + mae
    }
    mae = mae/kk
    return(mae)
}


avg.tae.norm = function(kk, n, m){
    mm = m
    nn = n
    tae = 0
    nn.vec=rep(nn+1, mm-1)
    l.vec=rep(0, mm-1)
    cn.vec=cumprod(nn.vec)
    cn.vec=c(1, cn.vec[-(mm-1)])
    cn.vec=cn.vec[length(cn.vec):1]
    cn.vec=as.integer(cn.vec)
    nnt=prod(nn.vec)
    for(i in 1:kk){
      pp = pmatrix(n,m)
      res0 = pmd(pp)
      res_normal=double(nnt)
      for(ii in 1:nnt)
      {
        idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
        #print(idx)
        if(nn-sum(idx-1)<0)
          res_normal[ii] = 0
        else
        {
          vec = c(idx-1,nn-sum(idx-1))
          res_normal[ii] = pmd(pp, x = vec, method = 'NA')
        }
      }
      ac = abs(res0 - res_normal)
      #if(res1 < ac)
      tae = sum(ac) + tae
    }
    tae = tae/kk
    return(tae)
}



f.mae.norm = function(n,m){
  pp = pmatrix(n,m)
  print(pp)
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
  return(mae)
}


f.tae.norm = function(n,m){
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
  tae = sum(abs(res0-res_normal))
  return(tae)
}


######################



library(doParallel)
library(foreach)
library(PMD)
print("NormM34TaeMae")



# mae normal m=3


n = c(1:100)
m = 3
N <- length(n)
results <- matrix(0,nrow = 1 ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE")

print("m=3,normal,mae")
detectCores()
cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  results$`n` = n[i]
  mae = 0
  mm = m
  nn = n[i]
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)

  # parallel here
  mae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.mae.norm(n[i],m)
  }
  mae = sum(mae)/K
  results$`MAE` = mae
  print(results)
  
}


# mae normal m=4


K= 100
n = c(1:100)
m = 4
N <- length(n)
results <- matrix(0,nrow = 1 ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE")

print("m=4,normal,mae")

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
  mae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.mae.norm(n[i],m)
  }
  mae = sum(mae)/K
  results$`MAE` = mae
  print(results)
  
}




#####################TAE normal m=3



K = 10
n = c(1:10)
m = 3
N <- length(n)
results <- matrix(0,nrow = N ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","TAE")

temp <- matrix(0,nrow = N ,ncol = 2)
temp <- as.data.frame(temp)
colnames(temp) <- c("n","TAE")

print("m=3,normal,tae")


cl = makeCluster(4)
registerDoParallel(cl)


for(i in 1:N){
  results$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  results$`TAE`[i] = tae
  print(results[i,])
  
}

stopCluster(cl)
print("__________________________________")



cl = makeCluster(4)
registerDoParallel(cl)

for(i in 1:N){
  results$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

stopCluster(cl)

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")



cl = makeCluster(4)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)

cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)




cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)



cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)

results$`TAE` = results$`TAE`/10
results




########################################## TAE m=4 normal


K = 1000
n = c(1:100)
m = 4
N <- length(n)
results <- matrix(0,nrow = N ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","TAE")

temp <- matrix(0,nrow = N ,ncol = 2)
temp <- as.data.frame(temp)
colnames(temp) <- c("n","TAE")

print("m=4,normal,tae")
detectCores()

cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  results$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  results$`TAE`[i] = tae
  print(results[i,])
  
}

stopCluster(cl)

print("__________________________________")



cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  results$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

stopCluster(cl)

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")



cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)

cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)




cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)



cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)


cl = makeCluster(100)
registerDoParallel(cl)

for(i in 1:N){
  temp$`n`[i] = n[i]
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
  tae = foreach(j=1:K,.export = 'dpmd',.packages = 'PMD',.combine = 'rbind') %dopar% {
    f.tae.norm(n[i],m)
  }
  tae = sum(tae)/K
  temp$`TAE`[i] = tae
  print(temp[i,])
  
}

results$`TAE` = results$`TAE` + temp$`TAE`  
print("__________________________________")
stopCluster(cl)

results$`TAE` = results$`TAE`/10
results
