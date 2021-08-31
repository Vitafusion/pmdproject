make.data.equal.groups = function(dat, groups_number,category_name=c("setosa","versicolor","virginica"),categor_column = 'Species'){
  n = nrow(dat)
  group = list()
  if(n %% groups_number != 0)
    stop("invalid number for grouping.")
  out = matrix(NA,nrow = groups_number,ncol = length(category_name))
  ss <- sample(1:groups_number,size=n,replace=TRUE,prob=rep(1/groups_number,groups_number))
  for (i in 1:groups_number) {
    group[[i]] = dat[ss==i,]
    expr = paste(categor_column)
    out[i,1] = nrow(group[[i]][which(group[[i]][,expr]==category_name[1]),])
    out[i,2] = nrow(group[[i]][which(group[[i]][,expr]==category_name[2]),])
    out[i,3] = nrow(group[[i]][which(group[[i]][,expr]==category_name[3]),])
  }
  return(list(group,out))
}

cal_pmatrix = function(parm, x_mat, category_number){
  n = ncol(x_mat)
  m = category_number
  parm = matrix(parm,n,m-1)
  P = matrix(0, nrow = nrow(x_mat), ncol = category_number)
  P = exp(x_mat%*%parm)/(rowSums(exp(x_mat%*%parm))+1)
  P = cbind(P,1/(rowSums(exp(x_mat%*%parm))+1))
  return(P)
}
####################################### Using groups to analysis likelihoods
point.loglik.calcu = function(pp,count_result){
  #browser()
  res = pmd(pp)
  l.vec = compute_l.vec(count_result)
  l.vec = paste(l.vec,collapse = ",")
  expr0 = "res["
  expr = paste0(expr0,l.vec,"]")
  likel = eval(parse(text=expr))
  log_lik = log(likel)
  #browser()
  return(log_lik)
}

toltal.loglik.calcu = function(parm,result_mat = count_result,group = groups,cat_number = category_number,covariate_num=4) {
  minus_log_lik = 0
  for (i in 1:nrow(result_mat)) {
    x = group[[i]]
    x_mat =as.matrix(x[,1:covariate_num])
    n = nrow(x)
    result = result_mat[i,]
    P = cal_pmatrix(parm, x_mat, cat_number)
    minus_log_lik = minus_log_lik - point.loglik.calcu(P,result)
    #browser()
  }
  #browser()
  return(minus_log_lik)
}



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