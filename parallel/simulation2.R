q.find <- function(res,q){
  res.temp <- res[res!=0]
  q.temp <- quantile(res.temp,q)
  q.addr <- which(res==q.temp)
  if(length(q.addr)==0){
    dis <- abs(res.temp-q.temp)
    temp.addr <- which(dis==min(dis))[1]
    q.addr <- which(res==res.temp[temp.addr])
  }
  
  return(q.addr)
}


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



library(poissonmulti)

K <- 100
m <- 3
n <- c(1:10)
N <- length(n)
b <- c(10^6, 10^7)
results <- matrix(0,nrow = 1, ncol = 9)
results <- as.data.frame(results)
colnames(results) <- c("n","m","B","max","err.max","per.95","err.95","per.90","err.90")
temp <- results
temp2 <- temp



print("b from 10^6 to 10^7")


for (j in 1:length(b)) {
  for (i in 1:N) {
    temp2$B <- b[j]
    temp2$n <- 10*i
    temp2$m <- m
    for(k in 1:K){
      pp <- p.matrix(10*i,m)
      res0 <- pmd(pp)
      res1 <- pmd(pp, method = "simulation", t=b[j])
      
      index.max <- q.find(res0,1)
      index.95 <- q.find(res0,0.95)
      index.90 <-  q.find(res0,0.90)
      err.max <- abs(res0[index.max] - res1[index.max])
      err.95 <- abs(res0[index.95] - res1[index.95])
      err.90 <- abs(res0[index.90] - res1[index.90])
      temp$max <- res0[index.max]
      temp$err.max <- err.max
      temp$`per.95` <- res0[index.95]
      temp$err.95 <- err.95
      temp$`per.90` <- res0[index.90]
      temp$err.90 <- err.90
      temp2[,4:9] <- temp2[,4:9] + temp[,4:9]
    }
    temp2[,4:9] <- temp2[,4:9]/K
    results <- rbind(results,temp2)
    print(results)
  }
}


results

write.table(results, 'results2.txt', sep='\t')