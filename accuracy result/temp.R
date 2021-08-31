library(mvtnorm)


p = pmatrix(30,4)
write.csv(p,'p.csv')
sig = sigma.calcu(p)
mu = mu.calcu(p)

sigma.calcu = function(p){
  n = nrow(p)
  m = ncol(p)-1
  if(n==1) pp = t(as.matrix(p[,1:m])) else pp = as.matrix(p[,1:m])
  sig = matrix(0,m,m)
  for (i in 1:n) {
    sig = sig + diag(pp[i,],nrow = m) - pp[i,]%*%t(pp[i,])
  }
  return(sig)
}

mu.calcu = function(p){
  n = nrow(p)
  m = ncol(p)-1
  if(n==1)  pp = t(as.matrix(p[,1:m])) else pp = as.matrix(p[,1:m])
  mu = matrix(0, nrow = 1, ncol = ncol(pp))
  for (i in 1:n) {
    mu = mu + pp[i,]
  }
  return(as.vector(mu))
}


pmvnorm(lower=c(9.5,9.5),upper = c(10.5,10.5), mean = mu, sigma = sig)
pmvnorm(lower=c(10.5,10.5),upper = c(11.5,11.5), mean = mu, sigma = sig)
pmvnorm(lower=c(8.5,8.5),upper = c(9.5,9.5), mean = mu, sigma = sig)


pnorm(14.5, mean = mu, sd = sqrt(sig),lower.tail = F)-pnorm(15.5, mean = mu, sd = sqrt(sig),lower.tail = F)
pnorm(15.5, mean = mu, sd = sqrt(sig),lower.tail = F)-pnorm(16.5, mean = mu, sd = sqrt(sig),lower.tail = F)
pnorm(16.5, mean = mu, sd = sqrt(sig),lower.tail = F)-pnorm(17.5, mean = mu, sd = sqrt(sig),lower.tail = F)
pnorm(17.5, mean = mu, sd = sqrt(sig),lower.tail = F)-pnorm(18.5, mean = mu, sd = sqrt(sig),lower.tail = F)
pnorm(18.5, mean = mu, sd = sqrt(sig),lower.tail = F)-pnorm(19.5, mean = mu, sd = sqrt(sig),lower.tail = F)


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
  #print(idx)
  if(nn-sum(idx-1)<0)
    res_normal[ii] = 0
  else
  {  
    vec = c(idx-1,nn-sum(idx-1))
    res_normal[ii] = pnorm()
  }
}


}