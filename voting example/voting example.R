compute_l.vec = function(result){
  m = length(result)
  res = result[1:(m-1)]
  l.vec = as.matrix(0,1,m-1)
  for (i in 1:(m-1)) {
    l.vec[i] = as.numeric(res[i]) + 1
  }
  return(l.vec)
}

cal_pmatrix = function(parm, x, n, m){
  if(nrow(x)!=n) return("fail")
  P = matrix(0, nrow = n, ncol = m)
  I = matrix(1, 1, m)
  for (i in 1:n) {
    for (j in 1:(m)) {
      P[i,j] = exp(parm[j,]%*%(x[i,]))/ (I %*% exp(parm%*%(x[i,])))
    }
  }
  return(P)
}



t = 
n = length(vot_dat)
m = 3
x = xmat
par=matrix(rep(0.01,6), 3, 6)
parm = par


setwd("C:/Users/linzh/Desktop/pmd/voting example") # need to be changed
library(dplyr)

rawdat <- read.table("ncvoter1.txt",header = T, fill = T, sep = '')
colnames(rawdat) = rawdat[1,]
dat = rawdat %>% select(party_cd)
dat = dat[-1,]
#res_city_desc race_code ethnic_code party_cd  gender_code birth_age  birth_state township_desc school_dist_desc munic_dist_desc  

dat = dat[-1,] %>% 
  mutate(res_city_desc = na_if(res_city_desc, ""))
dat = dat[complete.cases(dat),]

25575 + 23212 + 21844

index = sample(c(1:length(dat)),70631,replace = F)
vot_dat = dat[index]


name = unique(dat$res_city_desc)
name = name[2:length(name)]
l = length(name)
groups = vector(mode = "list", length = l)
expr0 = "groups["
for (i in 1:l) {
  expr = paste0(expr0, "[", i, "]]")
  expr = paste0(expr,"=vot_dat %>% filter(res_city_desc=='",name[i],"')")
  eval(parse(text=expr))
  
}
for (i in 1:l) {
  groups[[i]] = groups[[i]] %>% select(party_cd)
}


# beta matrix is 3x6

norm_likl = function(parm){
  m = 3
  n = length(vot_dat)
  aa=1:length(vot_dat)
  bb=as.matrix(vot_dat)
  fit.lm=lm(aa~factor(bb))  
  xmat=model.matrix(fit.lm)
  P = cal_pmatrix(parm,xmat,n,m)
  likeli = pmd(P,method = 'NA',vec = c(25575, 23212, 21844))
  return(likeli)
}

op = optim(
  par=matrix(c(2:19/1000), 3, 6),
  fn = norm_likl
  ,
  control = list(fnscale=-1)
)
op

parm = op$par

