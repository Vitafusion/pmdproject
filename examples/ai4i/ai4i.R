
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



cal_pmatrix = function(parm, x_mat, cat_number){
  x_mat = as.matrix(x_mat)
  
  n = ncol(x_mat) - 1
  #x_mat = cbind(matrix(1, nrow = nrow(x_mat),ncol = 1), x_mat)
  
  m = cat_number
  parm = matrix(parm,n+1,m-1, byrow=T)
  
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



toltal.loglik.calcu.ai4i = function(parm,
                                    result_mat = count_result,
                                    group = groups,
                                    cat_number = category_number,
                                    covariate_name = c("Air temperature [K]","Process temperature [K]","Rotational speed [rpm]")) {
  minus_log_lik = 0
  for (i in 1:length(result_mat)) {
    x = group[[i]]
    if(is.na(covariate_name)) {
    x_mat = matrix(1, nrow = nrow(x), ncol = 1)
    } else { 
      x_mat = x[,covariate_name] 
    } 
    
    x_mat = as.matrix(x_mat)
    x_mat = cbind(matrix(1, nrow = nrow(x_mat),ncol = 1), x_mat)


    result = result_mat[[i]]
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
raw.ai4i = read.table(file = 'ai4i2020.csv',sep = ',')
colname.ai4i <- raw.ai4i[1,]
colnames(raw.ai4i) <- c('UDI', 'Product ID', 'Type', 'Air temperature [K]', 'Process temperature [K]',
'Rotational speed [rpm]', 'Torque [Nm]', 'Tool wear [min]', 'Machine failure', 'TWF', 'HDF', 'PWF', 'OSF', 'RNF')
raw.ai4i <- raw.ai4i[-1,]

raw.ai4i[,4:14] <- apply(raw.ai4i[,4:14], 2, as.numeric)
raw.ai4i[,9:14] <- apply(raw.ai4i[,9:14], 2, as.numeric)
#ai4i <- raw.ai4i[which(raw.ai4i$`Machine failure`==1),]
ai4i <- raw.ai4i

ai4i$`Air temperature [K]` <- scale(ai4i$`Air temperature [K]`)
ai4i$`Process temperature [K]` <- scale(ai4i$`Process temperature [K]`)
ai4i$`Rotational speed [rpm]` <- scale(ai4i$`Rotational speed [rpm]`)
ai4i$`Torque [Nm]` <- scale(ai4i$`Torque [Nm]`)



# grouping based on unique combination of `Type` and `Tool wear [min]`
rule.group <- unique(ai4i[,c('Type','Tool wear [min]')])
groups <- list()
k <- 1
for(i in 1:nrow(rule.group)){
  type <- rule.group[i,1]
  toolwear <- rule.group[i,2]
  idx <- which(ai4i$Type==type & ai4i$`Tool wear [min]`==toolwear)
  groups[[k]] <- ai4i[idx,]
  k <- k+1
}

# Zero options
# result counts for each group
# data clean: remove all units that have mixed failures of category 1 and 2
# category 1: (TWF=1 or OSF=1) no others
# category 2: (HDF=1 or PWF=1) no others
# category 3: censored (non failure and others)


# data clean
ai4i$bool <- rep(0, nrow(ai4i))
for(i in 1:nrow(ai4i)){
  temp <- ai4i[i,c('TWF','HDF','PWF','OSF')]
  con1 <- sum(temp)>2
  con2 <- (temp$TWF==1 & temp$HDF==1)
  con3 <- (temp$TWF==1 & temp$PWF==1)
  con4 <- (temp$OSF==1 & temp$HDF==1)
  con5 <- (temp$OSF==1 & temp$PWF==1)
  cond <- c(con1, con2, con3, con4, con5)
  
  if(any(cond)==T)
    ai4i$bool[i] <- 1
}

#ai4i <- ai4i[which(ai4i$bool==0),]

library(dplyr)
#plot of distribution for each category
h1 <- ai4i[which((ai4i$TWF==1 | ai4i$OSF==1)&(ai4i$HDF==0 & ai4i$PWF==0)),]
h2 <- ai4i[which((ai4i$HDF==1 | ai4i$PWF==1)&(ai4i$TWF==0 & ai4i$OSF==0)),]
h3 <- setdiff(ai4i,rbind(h1,h2))
mat.hist <- matrix(c(nrow(h1), nrow(h2), nrow(h3)), nrow=1, byrow = T)
colnames(mat.hist) <- c('TWF_OSF','HDF_PWF','other')
plot(mat.hist[1,])


t1 <- mat.hist[1,]
t2 <- c('TWF_OSF','HDF_PWF','other')
mat <- t(rbind(t1,t2))
mat <- as.data.frame(mat)
mat$t1 <- as.numeric(mat$t1)
colnames(mat) <- c('counts', 'category')

library(ggplot2)
ggplot(mat, aes(category, counts)) +
  geom_bar(stat = 'identity')   

# generateing result counts
res.count <- list()
for (i in 1:nrow(rule.group)) {
  #if(nrow(groups[[i]]) != 1)
    for(j in nrow(groups[[i]])){
      temp.dat <- groups[[i]]
      c1 <- c2 <- c3 <- 0
      for(k in 1:nrow(temp.dat)){
        temp <- temp.dat[k,]
        if((temp$TWF==1 | temp$OSF==1)&(temp$HDF==0 & temp$PWF==0))
          c1 <- c1+1
        else if((temp$TWF==0 & temp$OSF==0)&(temp$HDF==1 | temp$PWF==1))
          c2 <- c2+1
        else
          c3 <- c3+1
      }
    }
  res.count[[i]] <- c(c1,c2,c3)
}


 

# First Option
# result count for each group
# category 1: (TWF=1 OR OSF=1) AND (HDF=PWF=0)
# category 2: (TWF=0 AND OSF=0) AND (HDF=1 or PWF=1)
# category 3: else (including non failure)
# 
# res.count <- list()
# for (i in 1:nrow(rule.group)) {
#   if(nrow(groups[[i]]) != 1)
#     for(j in nrow(groups[[i]])){
#       temp.dat <- groups[[i]]
#       c1 <- c2 <- c3<- 0
#       for(k in 1:nrow(temp.dat)){
#         temp <- temp.dat[k,]
#         if((temp$TWF==1 | temp$OSF==1) & temp$HDF==0 & temp$PWF==0)
#         c1 <- c1+1
#         else if(temp$TWF==0 & temp$OSF==0 & (temp$HDF==1 | temp$PWF==1))
#           c2 <- c2+1
#         else
#           c3 <- c3+1
#       }
#       
#     }
#   res.count[[i]] <- c(c1,c2,c3)
# }


# Second Option
# result count for each group
# category 1: 0 failure mode
# category 2: 1 failure modes
# category 3: 1+ failure modes
# 
# res.count <- list()
# for (i in 1:nrow(rule.group)) {
#   #if(nrow(groups[[i]]) != 1)
#       temp.dat <- groups[[i]]
#       c1 <- c2 <- c3 <- 0
#       for(k in 1:nrow(temp.dat)){
#         twf <- osf <- hdf <- pwf <- rnf <- 0
#         temp <- temp.dat[k,]
#         if(temp$RNF==1)
#           rnf <- 1
#         if(temp$TWF==1)
#           twf <- 1
#         if(temp$OSF==1)
#           osf <- 1
#         if(temp$HDF==1)
#           hdf <- 1
#         if(temp$PWF==1)
#           pwf <- 1
#         
#         s <- twf+osf+hdf+pwf+rnf
#         if (s>=2) {
#           c3 <- c3 + 1
#         } else if (s==1) {
#           c2 <- c2+1
#         } else {
#           c1 <- c1+1
#         } 
#       }
#   #if(c1+c2+c3!= nrow(groups[[i]]))
#   # cat(c1+c2+c3,",", nrow(groups[[i]])," , ",i, "\n")
#   res.count[[i]] <- c(c1,c2,c3)
# }


# test: find out the group that has largest c2 or c3
# c1.res <- c2.res <-c3.res <- c()
# s <- 0
# for(i in 1:nrow(rule.group)){
#   c1.res <- c(c1.res, res.count[[i]][1])
#   c2.res <- c(c2.res, res.count[[i]][2])
#   c3.res <- c(c3.res, res.count[[i]][3])
#   s <- s + sum(res.count[[i]]) 
# }
# 
# 
# s <- 0
# for(i in 1:nrow(rule.group)){
#   s <- s + sum(nrow(groups[[i]])) 
# }

# ai4i = ai4i[-1,2:7]
# ai4i$Type=as.factor(ai4i$Type)
# for(i in 3:6){
#   ai4i[,i] = as.character(ai4i[,i])
#   ai4i[,i] = as.numeric(ai4i[,i])
#   ai4i[,i] = scale(ai4i[,i])
# }
# ai4i = ai4i[1:1000,] 




covariates <- c("Air temperature [K]", "Process temperature [K]", "Rotational speed [rpm]", "Torque [Nm]")
seed <- 1
covidx=c(1,2,3,4)
# category number 3
m <- 3

## read in the command line arguments
## run with: R CMD BATCH '--args seed=1 covidx=c(1,2,3,4,5)' ai4i.R
args <- commandArgs(TRUE)
if(length(args) > 0) 
    for(i in 1:length(args)) 
        eval(parse(text=args[[i]]))

## print seed
cat("covariates index: ", covidx, "\n", sep="")
cat("seed is ", seed, "\n", sep="")


covname <- c()
for(i in 1:length(covidx)){
  if(covidx[i]==0) {
    covname <- NA
  } else { 
    covname <- c(covname, covariates[covidx[i]])
  }
}

cat("covname: ", covname, "\n", sep="")



library(PoissonMultinomial)
f = function(parm){
  toltal.loglik.calcu.ai4i(parm,
                           result_mat = res.count,
                           group = groups,
                           cat_number = 3,
                           covariate_name = covname)
}



# optim to find estimates for betas, m categories and k covariates
# so beta(including intercep) will be a (m-1)*(k+1) matrix
if(is.na(covname[1])){ 
  parm <- rep(1,2)
} else { 
  parm <- c(1, 1, rep(0, (m-1)*(length(covname))))
}

op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
op 



h <- op$hessian
hinv <- solve(h)

if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_7.RData')
}


h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_6.RData')
}



h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_5.RData')
}


h <- op$hessian
hinv <- solve(h)
if(any(diag(hinv) < 0)){
  parm <- op$par
  op <- optim(
  parm,
  f,
  method = "Nelder-Mead",
  hessian = T,
  control = list(trace = T,maxit = 30000)
)
} else {
  save(op,h,hinv, file='ai4i_4.RData')
}




# #parm <- c(7.142795, -3.243091,  1.927554,  3.688145, -2.688194,  1.608610)

# # estimated beta
# beta.hat <- op$par

# # the hessian and inverse of hessian
# H <- op$hessian
# H.inv <- solve(H)


# # se of beta

# se <- sqrt(diag(H.inv))

# # 0.95 CI
# left.CI <- beta.hat - 1.96*se
# right.CI <- beta.hat + 1.96*se


# # calculate p matrix for all groups
# pp = list()
# covariate_name = c("Air temperature [K]",
#                    "Process temperature [K]",
#                    "Rotational speed [rpm]",
#                    "Torque [Nm]")
# category_number = 3
# for(i in 1:length(groups)) {
#   x = groups[[i]]
#   x = x[,covariate_name]
#   pp[[i]]= cal_pmatrix(op$par, x, category_number)
  
# }

# # P matrix for group 1, 5 and 8
# pp[[1]]
# pp[[5]]
# pp[[8]]


# save results

expr <- paste0('ai4i_', seed, '.RData')
save(op, beta.hat, H, file=expr)


