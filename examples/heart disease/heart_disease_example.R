#setwd("C:/Users/linzh/Desktop/pmd/heart disease")
library(dplyr)
dyn.load("functions.so")
source('myfunctions.R')
################################### data prepare
clev_raw <- read.table('processed.cleveland.data',sep = ',')
colnames(clev_raw) <- c("age",
                        "sex",
                        "cp",
                        "trestbps",
                        "chol",
                        "fbs",
                        "restecg",
                        "thalach",
                        "exang",
                        "oldpeak",
                        "slope",
                        "ca",
                        "thal",
                        "num")
dat <- clev_raw[1:300,]
dat = dat %>% select(oldpeak,slope,chol,age,num)
cov_num = 4
#oldpeak,slope,chole,age,trestbps totally 5 covariates

#exang: exercise induced angina
#cp: chest pain type
#restecg: resting electrocardiographic results
#slope: the slope of the peak exercise ST segment


res = make.data.equal.groups(dat, 30,category_name=c(0,1,2,3,4),category_column = 'num')
groups = res[[1]]
count_result = res[[2]]
category_number = 5 

f = function(parm){
  toltal.loglik.calcu(parm,result_mat = count_result,group = groups,cat_number = category_number,cov_num)
}

########################################### optimize likelihood
parm=rep(0.02,4*4) #5 categories, 4 covariates
op = optim(
  parm,
  f,
  method = "BFGS",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op



cal_pmatrix(parm, groups[[1]], category_number)
