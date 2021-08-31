#simulation accuracy test, n=3
#we set m = 15 here
#additionaly we set repeating time t = 10^6 for time efficiency
dyn.load('functions.so')
source('myfunctions.R')
print('n=5')

n = 5
m = c(2:10)
N <- length(m)
results <- matrix(0,nrow = N ,ncol = 4)
results <- as.data.frame(results)
colnames(results) <- c("m","(p_min,p_max)","MAE","TAE")


for (i in 1:N) {
  pp = pmatrix(n,m[i]) 
  mm = m[i]
  nn = n
  results$`(p_min,p_max)`[i] <- paste0("(",round(min(pp),2),",",round(max(pp),2),")")
  results$`m`[i] <-  m[i]
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
  res1 = pmd(pp,method = 'simulation',t = 10^6)
  ac = abs(res0-res1)
  tae = sum(ac)
  mae = max(ac)
  results$TAE[i] = formatC(tae, format = "e", digits = 5)
  results$MAE[i] = formatC(mae,format = 'e',digits = 5)
  print(results[i,])
}
results


write.csv(results,"simulation_accuracy_n=5_t=10^6.cvs")