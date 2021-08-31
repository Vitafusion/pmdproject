setwd("C:/Users/linzh/Desktop/pmd/examples/simulate data")

library(dplyr)
library(tidyr)


dat = read.table(file = 'beta.txt',sep = ',')
beta = matrix(NA,nrow=1000,ncol=4)
j = 1
for(i in 1:nrow(dat)){
  temp = strsplit(dat[i,],' ')[[1]][1]
  if(temp=='[1]')
  {  beta[j,] = as.numeric(strsplit(dat[i,],' ')[[1]][2:5])
    j = j+1
  }
}

hist(beta[,1])
hist(beta[,2])
hist(beta[,3])
hist(beta[,4])
