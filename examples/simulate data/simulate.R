#setwd("C:/Users/linzh/Desktop/pmd/examples/simulate data")
#source("C:/Users/linzh/Desktop/pmd/functions/myfunctions.R")
#dyn.load('C:/Users/linzh/Desktop/pmd/functions/functions.so')

source("/home/zhengzhi/functions/myfunctions.R")
dyn.load("/home/zhengzhi/functions/functions.so")

beta = matrix(NA,nrow = 1000,ncol = 4)
for (i in 1:1000) {
  data.generator = function(cov_num,N,m,beta){
    parm = matrix(beta,nrow=m-1,ncol=cov_num)
    x1 = rnorm(N)
    x2 = rnorm(N)
    x = as.matrix(cbind(x1,x2))
    P = exp(x%*%parm)/(rowSums(exp(x%*%parm))+1)
    P = cbind(P,1/(rowSums(exp(x%*%parm))+1))
    choice = apply(P,1,rmultinom,n=1,size=1)
    choice = t(choice)
    y = apply(choice,1,function(x) which(x==1))
    y = as.data.frame(y)
    return(list(x,y,P))
    
  }
  r = data.generator(2,5000,3,beta = c(0.5,0.6,0.7,0.8))
  dat_simu = as.data.frame(cbind(r[[1]],r[[2]]))
  out = make.data.equal.groups(dat_simu,500,category_name = c(1,2,3),category_column = 'y')
  groups = out[[1]]
  count_result = out[[2]]
  category_number = 3
  f = function(parm){
    toltal.loglik.calcu(parm,result_mat = count_result,group = groups,cat_number = category_number,covariate_num = 2)
  }
  
  ########################################### optimize likelihood
  parm=c(0.5,0.5,0.5,0.5) 
  op = optim(
    parm,
    f,
    method = "BFGS",
    hessian = T,
    control = list(trace = T,maxit = 30000)
  )
  op 
  beta[i,] = op$par
  print(beta[i,])
}