#n = 100 here
dyn.load('functions.so')
source('myfunctions.R')
print('m=5,sparsity')

n = c(1:100)
m = 5
N <- length(n)
results <- matrix(0,nrow = N ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE")


for (i in 1:N) {
  results$`n`[i] =  n[i]
  mae = 0
  for (k in 1:100) {
    pp = pmatrix(n[i],m) 
    mm = m
    nn = n[i]
    nn.vec=rep(nn+1, mm-1)
    l.vec=rep(0, mm-1)
    cn.vec=cumprod(nn.vec)
    cn.vec=c(1, cn.vec[-(mm-1)])
    cn.vec=cn.vec[length(cn.vec):1]
    cn.vec=as.integer(cn.vec)
    nnt=prod(nn.vec)
    res0 = pmd(pp)
    mae = mae + max(res0)
  }
  results$MAE[i] = formatC(mae/200,format = 'e',digits = 5)
  print(results[i,])
  
}
results
write.csv(results,"spar_m=5_r=100.csv")