# test likelihood 
#setwd("C:/Users/linzh/Desktop/pmd/heart disease")
library(dplyr)
dyn.load("functions.so")
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
dat <- clev_raw
dat = dat %>% select(-ca,-thal)
dat <- dat  %>% mutate(cat0 = ifelse(num==0,1,0),
                       cat1 = ifelse(num==1,1,0),
                       cat2 = ifelse(num==2,1,0),
                       cat3 = ifelse(num==3,1,0),
                       cat4 = ifelse(num==4,1,0)) %>% select(-fbs)

grouped_dat <- dat %>% group_by(cp,sex,exang) %>% summarise(count0 = sum(cat0),
                                                            count1 = sum(cat1),
                                                            count2 = sum(cat2),
                                                            count3 = sum(cat3),
                                                            count4 = sum(cat4))

###################################### Creating groups
groups = vector(mode = "list", length = nrow(grouped_dat))
expr0 = "groups["
for (i in 1:nrow(grouped_dat)) {
  vec = as.vector(grouped_dat[i,] %>% select(cp,sex,exang))
  expr = paste0(expr0, "[", i, "]]")
  expr = paste0(expr,"=dat %>% filter(cp==vec$cp& sex==vec$sex& exang==vec$exang)")
  eval(parse(text=expr))
  
}
for (i in 1:nrow(grouped_dat)) {
  groups[[i]] = groups[[i]] %>% select(-cp,-sex,-exang,-cat0,-cat1,-cat2,-cat3,-cat4,-num)
}
####################################################################
###########################


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

pmatrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r <- runif(m)
    r <- r/sum(r) #generate row
    p[i,] <- r
  }
  return(p)
}

pmd <-function(pp,method="DFT-CF",vec=c(0,0,0,0,0),t=100)
{
  
  if(any(pp<0)|any(pp>1))
  {
    stop("invalid values in pp.")
  }
  
  
  switch(method,
         "DFT-CF"={
           mm=ncol(pp) # m categories
           nn=nrow(pp) # n people
           
           nn.vec=rep(nn+1, mm-1)
           l.vec=rep(0, mm-1)
           cn.vec=cumprod(nn.vec)
           cn.vec=c(1, cn.vec[-(mm-1)])
           cn.vec=cn.vec[length(cn.vec):1]
           cn.vec=as.integer(cn.vec)
           
           nnt=prod(nn.vec)
           res0=double(nnt)
           
           #browser()
           
           tmp=.C("pmn_mdfft", as.double(res0), as.integer(nnt), as.integer(nn), as.integer(mm), as.double(pp), as.integer(nn.vec), as.integer(l.vec), as.integer(cn.vec))
           res0=tmp[[1]]
           #example an_array[k + 27 * (j + 12 * i)]
           #print(round(res0, 9))
           res=array(0, nn.vec)
           
           res.expr="res[idx[1]"
           if(mm>=3)
           {
             for(i in 2:(mm-1))
             {
               res.expr=paste0(res.expr, ", idx[", i, "]")
             }
           }
           res.expr=paste0(res.expr, "]=res0[i]")
           
           #browser()
           
           #print(nnt)
           
           for(i in 1:nnt)
           {
             idx=l.vec.compute(k=i, cn.vec=cn.vec, m=mm)
             #print(idx)
             eval(parse(text=res.expr))
           }
           
           res=round(res, 10)
           return(res) 
           
         },
         "simulation"=    {
           mm=ncol(pp) # m categories
           nn=nrow(pp) # n people
           nn.vec=rep(nn+1, mm-1)
           l.vec=rep(0, mm-1)
           cn.vec=cumprod(nn.vec)
           cn.vec=c(1, cn.vec[-(mm-1)])
           cn.vec=cn.vec[length(cn.vec):1]
           cn.vec=as.integer(cn.vec)
           nnt=prod(nn.vec)
           res0=double(nnt)
           temp=.C("pmd_simulation", as.double(res0),
                   as.integer(nnt) , as.integer(nn), as.integer(mm), as.double(pp),
                   as.integer(nn.vec), as.integer(l.vec), as.integer(cn.vec), as.integer(t))
           res=round(temp[[1]],10)
         },
         "NA"=   {
           mm=ncol(pp) # m categories
           nn=nrow(pp) # n people
           if(sum(vec)>nn|any(vec<0)|length(vec)!=mm)
           {
             stop("invalid result vector")
           }
           mm = mm - 1
           x_vec = vec[1:(length(vec)-1)]
           res = 0
           temp=.C("pmd_normal",as.double(res), as.integer(nn), as.integer(mm), as.double(pp),as.integer(x_vec))
           res = temp[[1]]
         }
         
  )
  
  return(res)
}


##################################################### calculate P matrix

cal_pmatrix = function(parm, x, n, m){
  if(nrow(x)!=n) return("fail")
  P = matrix(0, nrow = n, ncol = m)
  I = matrix(1, 1, (m-1))
  for (i in 1:n) {
    for (j in 1:(m-1)) {
      P[i,j] = exp(parm[j,]%*%t(x[i,]))/ (1 + I %*% exp(parm%*%t(x[i,])))
    }
  }
  for (i in 1:n) {
    P[i,m] = 1 - sum(P[i,1:(m-1)])
  }
  return(P)
}
######################### compute l.vec wrt result
compute_l.vec = function(result){
  m = length(result)
  res = result[1:(m-1)]
  l.vec = as.matrix(0,1,m-1)
  for (i in 1:(m-1)) {
    l.vec[i] = as.numeric(res[i]) + 1
  }
  return(l.vec)
}
################################### Using groups to analysis likelihoods

m = 5
parm = matrix(0,4,7)
  parm = matrix(parm, 4, 7) # Reshape
  likel = rep(0,nrow(grouped_dat))
  for (i in 1:nrow(grouped_dat)) {
    x = groups[[i]]
    n = nrow(x)
    P = cal_pmatrix(parm, x, n, m)
    res = pmd(P)
    res0 = grouped_dat[i, c('count0','count1','count2','count3','count4')] 
    l.vec = compute_l.vec(res0)
    l.vec = paste(l.vec,collapse = ",")
    expr0 = "res["
    expr = paste0(expr0,l.vec,"]")
    likel[i] = eval(parse(text=expr))
  }


