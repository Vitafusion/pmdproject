library(doParallel)
library(foreach)
library(poissonmulti)


K=1000
n = c(1:10)
m = 3
N <- length(n)
results <- matrix(0,nrow = 1 ,ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("n","TAE")

print("m=3")

cl = makeCluster(20)
registerDoParallel(cl)

foreach(i=1:N,.export='avg.tae',.packages='poissonmulti',.combine='rbind') %dopar% {results$`n` <-  n[i]
tae = avg.tae(kk=K,n=n[i],m)
results$TAE = formatC(tae, format = "e", digits = 5)
print(results)
}

stopCluster(cl)
