#setwd("C:/Users/linzh/Desktop/pmd/examples/iris")
data(iris)

dyn.load("functions.so")
source('myfunctions.R')

set.seed(200)
res = make.data.equal.groups(iris,30)
groups = res[[1]]
count_result = res[[2]]
category_number = 3
###########################

f = function(parm){
  toltal.loglik.calcu(parm,result_mat = count_result,group = groups,cat_number = category_number)
}

########################################### optimize likelihood
parm=rep(0.02,4*2) #3 categories, 4 covariates
op = optim(
  parm,
  f,
  method = "BFGS",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op

