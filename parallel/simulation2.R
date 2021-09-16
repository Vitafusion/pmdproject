q.find <- function(res,q){
  res.temp <- res[res!=0]
  q.temp <- quantile(res.temp,q)
  
  if(length(which(res==q.temp))>1){
    q.addr <- which(res==q.temp)[1]
  }
  else{
    q.addr <- which(res==q.temp)
  }

  if(length(q.addr)==0){
    dis <- abs(res.temp-q.temp)
    temp.addr <- which(dis==min(dis))[1]
    q.addr <- which(res==res.temp[temp.addr])[1]
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
colnames(results) <- c("n","m","B","max","err.max","per.75","err.75","per.50","err.50")
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
      index.75 <- q.find(res0,0.75)
      index.50 <-  q.find(res0,0.50)
      err.max <- abs(res0[index.max] - res1[index.max])
      err.75 <- abs(res0[index.75] - res1[index.75])
      err.50 <- abs(res0[index.50] - res1[index.50])
      temp$max <- res0[index.max]
      temp$err.max <- err.max
      temp$`per.75` <- res0[index.75]
      temp$err.75 <- err.75
      temp$`per.50` <- res0[index.50]
      temp$err.50 <- err.50
      temp2[,4:9] <- temp2[,4:9] + temp[,4:9]
    }
    temp2[,4:9] <- temp2[,4:9]/K
    results <- rbind(results,temp2)
    print(results)
  }
}


results

write.table(results, 'results752.txt', sep='\t')