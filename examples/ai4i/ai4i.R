
compute_l.vec = function(result){
  m = length(result)
  res = result[1:(m-1)]
  l.vec = rep(0,m-1)
  for (i in 1:(m-1)) {
    l.vec[i] = as.numeric(res[i]) + 1
  }
  return(l.vec)
}


l.vec.compute=function(k, cn.vec, m)
{
  k=k-1
  l.vec=rep(0, m-1)
  for(i in 1:(m-1))
  {
    aa=k%%cn.vec[i]
    bb=(k-aa)/cn.vec[i]
    l.vec[i]=bb
    k=aa
  }
  l.vec=l.vec+1
  return(l.vec)
}



make.data.groups = function(dat, groups_number,category_name=c("setosa","versicolor","virginica"),category_column = 'Species'){
  n = nrow(dat)
  group = list()
  k=1
  if(n %% groups_number != 0)
    stop("invalid number for grouping.")
  out = matrix(NA,nrow = groups_number,ncol = length(category_name))
  ss <- sample(1:groups_number,size=n,replace=TRUE,prob=rep(1/groups_number,groups_number))
  for (i in 1:groups_number) {
    if(nrow(dat[ss==i,])!=0){
      group[[k]] = dat[ss==i,]
      expr = paste(category_column)
      for(j in 1:length(category_name)){
        out[k,j] = nrow(group[[k]][which(group[[k]][,expr]==category_name[j]),])
      }
      k = k + 1
    }
  }  
  out = na.omit(out)
  return(list(group,out))
}



cal_pmatrix = function(parm, x_mat, category_number){
  x_mat = as.matrix(x_mat)
  n = ncol(x_mat)
  x_mat = cbind(matrix(1, nrow = nrow(x_mat),ncol = 1), x_mat)
  m = category_number
  parm = matrix(parm,n+1,m-1)
  #P = matrix(0, nrow = nrow(x_mat), ncol = category_number)
  P = exp(x_mat%*%parm)/(rowSums(exp(x_mat%*%parm))+1)
  P = cbind(P,1/(rowSums(exp(x_mat%*%parm))+1))
  for(i in 1:nrow(P)){
    P[i,1:(m-1)] = round(P[i,1:(m-1)],5)
    s = sum(P[i,1:(m-1)])
    if(s>=1){
      P[i,1:(m-1)] = P[i,1:(m-1)]/s
    }
    P[i,m] = 1 - sum(P[i,1:(m-1)])
  }
  return(P)
}


point.loglik.calcu = function(pp,count_result){
  #browser()
  res = dpmd(pp)
  l.vec = compute_l.vec(count_result)
  l.vec = paste(l.vec,collapse = ",")
  expr0 = "res["
  expr = paste0(expr0,l.vec,"]")
  likel = eval(parse(text=expr))
  log_lik = log(likel)
  #browser()
  return(log_lik)
}



toltal.loglik.calcu = function(parm, 
                               result_mat = count_result, 
                               group = groups,
                               cat_number = category_number,
                               covar.index) {
  minus_log_lik = 0
  for (i in 1:nrow(result_mat)) {
    x = group[[i]]
    x_mat = x[,covar.index]
    n = nrow(x)
    result = result_mat[i,]
    P = cal_pmatrix(parm, x_mat, cat_number)
    prob = point.loglik.calcu(P,result)
    if(prob == -Inf)
      prob = -10000
    minus_log_lik = minus_log_lik - prob
    #browser()
  }
  #browser()
  return(minus_log_lik)
}



toltal.loglik.calcu.ai4i = function(parm,
                                    result_mat = count_result,
                                    group = groups,
                                    cat_number = category_number,
                                    covariate_name = c("Air temperature [K]","Process temperature [K]","Rotational speed [rpm]")) {
  minus_log_lik = 0
  for (i in 1:nrow(result_mat)) {
    x = group[[i]]
    x_mat = x[,covariate_name]
    n = nrow(x)
    result = result_mat[i,]
    P = cal_pmatrix(parm, x_mat, cat_number)
    prob = point.loglik.calcu(P,result)
    if(prob == -Inf){
      prob = -10000
    }
    minus_log_lik = minus_log_lik - prob
    if(minus_log_lik==Inf){
      stop("!!")
    }
    #browser()
  }
  #browser()
  return(minus_log_lik)
}


#data prepare
ai4i = read.table(file = 'dat_ai4i.csv',sep = ',')
colnames(ai4i) = c('NA','Product ID','Type','Air temperature [K]','Process temperature [K]','Rotational speed [rpm]','Torque [Nm]')
#colnames(ai4i) = ai4i[1,]
ai4i = ai4i[-1,2:7]
ai4i$Type=as.factor(ai4i$Type)
for(i in 3:6){
  ai4i[,i] = as.character(ai4i[,i])
  ai4i[,i] = as.numeric(ai4i[,i])
  ai4i[,i] = scale(ai4i[,i])
}
ai4i = ai4i[1:1000,] 



set.seed(10000)
out = make.data.groups(ai4i,100,category_name = c('H','L','M'),category_column = 'Type')
groups = out[[1]]
count_result = out[[2]]
category_number = 3

idx = c()
for (i in 1:length(groups)) {
  idx[i] = nrow(groups[[i]])
}

min(idx)
max(idx)



f = function(parm){
  toltal.loglik.calcu.ai4i(parm,
                           result_mat = count_result,
                           group = groups,
                           cat_number = category_number,
                           covariate_name = c("Air temperature [K]","Process temperature [K]"))
}



# optim to find estimates for betas, 3 categories and 2 covariates
# so beta(includes intercep) will be a 2*3 matrix

parm=c(1,1,1,1,1.8,1.625)
op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op 


# estimated beta
beta.hat <- op$par

# the hessian and inverse of hessian
H <- op$hessian
H.inv <- solve(H)


# se of beta

se <- sqrt(diag(H.inv))

# 0.95 CI
left.CI <- beta.hat - 1.96*se
right.CI <- beta.hat + 1.96*se


# calculate p matrix for all groups
pp = list()
covariate_name = c("Air temperature [K]","Process temperature [K]")
category_number = 3
for(i in 1:length(groups)) {
  x = groups[[i]]
  x = x[,covariate_name]
  pp[[i]]= cal_pmatrix(op$par, x, category_number)
  
}

# P matrix for group 1, 5 and 8
pp[[1]]
pp[[5]]
pp[[8]]


# save results
save(ai4i, out, op, beta.hat, H, H.inv, se, left.CI, right.CI, pp, file="ai4i.RData")