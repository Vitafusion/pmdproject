source("C:/Users/linzh/Desktop/pmd/functions/myfunctions.r")

setwd("C:/Users/linzh/Desktop/pmd")

library(parallel)
library(MASS)
library(foreach)
library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
getDoParRegistered()

x <- vector()
for(i in 1:10){
  x[i] <- sqrt(i)
}
x <- foreach(i = 1:10,.combine = 'c') %dopar% {
  for (j in 1:4) {
    sqrt(i+j)
  }
}
x
stopCluster(cl = cl)


a <- list(c('for', 'lapply', 'doParallel::parLapply', 'doParallel::foreach', 'doParallel::mclapply'))
b<- list(c(55.4708, 57.00911, 19.38573, 14.87837, 42.62276))
result <- do.call(rbind, Map(data.frame, A=a, B=b))
