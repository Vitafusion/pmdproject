#simulation accuracy test, m = 5
#we set n = 100 here
#additionaly we set repeating time t = 10^4 for time efficiency
dyn.load('functions.so')
source('myfunctions.R')

n = c(1:100)
m = 5
N <- length(n)
results <- matrix(0,nrow = N ,ncol = 3)
results <- as.data.frame(results)
colnames(results) <- c("n","MAE","TAE")

temp_dat <- matrix(0,nrow = 100 ,ncol = 2)
temp_dat <- as.data.frame(temp_dat)
colnames(temp_dat) <- c("MAE","TAE")

print("m=5")

for (i in 1:N) {
  results$`n`[i] <-  n[i]
  for(k in 1:100){
    pp = pmatrix(n[i],m) 
    mm = m
    nn = n[i]
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
    res1 = pmd(pp,method = 'simulation',t = 10^4)
    ac = abs(res0-res1)
    tae = sum(ac)
    mae = max(ac)
    temp_dat$MAE[k] = mae
    temp_dat$TAE[k] = tae
  }
  results$TAE[i] = formatC(mean(temp_dat$TAE), format = "e", digits = 5)
  results$MAE[i] = formatC(mean(temp_dat$MAE),format = 'e',digits = 5)
  print(results[i,])
}
results


write.csv(results,"m=5_t=10^4_r=100.csv")
