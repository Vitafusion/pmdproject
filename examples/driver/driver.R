#setwd("C:/Users/linzh/Desktop/pmd/examples/driver")
#source("C:/Users/linzh/Desktop/pmd/functions/myfunctions.R")



source("/home/zhengzhi/functions/myfunctions.R")
dyn.load("/home/zhengzhi/functions/functions.so")



driver = load.driver()


set.seed(10000)
out = make.data.groups(driver,800,
                       category_name = c("Crash","Near-Crash","Crash-Relevant","Non-Subject Conflict"),
                       category_column = 'event.severity')
groups = out[[1]]
count_result = out[[2]]
category_number = 4

f = function(parm){
  toltal.loglik.calcu.driver(parm,
                           result_mat = count_result,
                           group = groups,
                           cat_number = category_number,
                           covariate_name = c("event.start","reaction.start","impact.time","event.end"))
}


parm=rep(0.3,12)
op = optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op 

pp = list()
covariate_name = c("event.start","reaction.start","impact.time","event.end")
category_number = 4
for(i in 1:length(groups)) {
  x = groups[[i]]
  x = x[,covariate_name]
  pp[[i]]= cal_pmatrix(op$par, x, category_number)
  
}

groups[[1]]
groups[[5]]
groups[[8]]

pp[[1]]
pp[[5]]
pp[[8]]