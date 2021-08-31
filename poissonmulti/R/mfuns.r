#'probability density of Poisson multinomial distribution
#'
#' @param pp The pp is a probability matrix which will be input by user
#' @param vec result vec input by user
#' @param method method selected by user to compute the probability mass
#' @param t simulation repeat time
#' @return The probability mass of PMD
#' @export
#'

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

           tmp=.C("pmn_mdfft", as.double(res0), as.integer(nnt), as.integer(nn), as.integer(mm), as.double(pp), as.integer(nn.vec), as.integer(l.vec), as.integer(cn.vec), PACKAGE = "poissonmulti")
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
                   as.integer(nn.vec), as.integer(l.vec), as.integer(cn.vec), as.double(t),PACKAGE = "poissonmulti")
           res0=round(temp[[1]],10)
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
           temp=.C("pmd_normal",as.double(res), as.integer(nn), as.integer(mm), as.double(pp),as.integer(x_vec),PACKAGE = "poissonmulti")
           res = temp[[1]]
         }

  )

  return(res)
}
################################################################################

#' l.vec
#'
#' @param k input by given functions
#' @param cn.vec calculated by given fucntions
#' @param m dimensions
#'
#' @return A vector, which will be used in other functions
#' @export
#'


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
################################################################################
#' ppmn
#'
#' @param pp input matrix of probabilities
#' @param x input vector
#' @param method method
#' @param t repeating time
#' @return prob
#' @export
#'
ppmn = function(pp,x,method="DFT-CF",t=1000){
  if(any(pp<0)|any(pp>1)){
    stop("invalid values in pp.")
  }
  nn = nrow(pp)
  mm = ncol(pp)
  if(any(x<0)){
    stop("invalid values in x.")
  }
  if(length(x)!=mm){
    stop("invalid format of x.")
  }
  #idx formed
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  
  nnt=prod(nn.vec)
  idx = as.data.frame(matrix(0,nrow = nnt,ncol=mm))
  idx0 = as.data.frame(matrix(0,nrow = nnt,ncol=(mm-1)))
  #transfer l.vec to result vector(idx)
  for(i in 1:nnt)
  {
    idx[i,1:(mm-1)]=l.vec.compute(k=i, cn.vec=cn.vec, m=mm)
    idx0[i,] = l.vec.compute(k=i, cn.vec=cn.vec, m=mm)
    idx[i,1:(mm-1)] = idx[i,1:(mm-1)]-1
    idx[i,mm] = nn - sum(idx[i,1:(mm-1)])
    #print(idx)
  }
  
  # filter density points
  conditions = c()
  expr0 = 'which('
  expr1 = ')'
  for(i in 1:mm){
    conditions[i] = paste0('idx$V',i,'<=',x[i])
  }
  cond = conditions[1]
  for(i in 1:(mm-1)){
  cond = paste0(cond,'&',conditions[i+1])
  }
  expr = paste0(expr0,cond,expr1)
  index = eval(parse(text=expr))
  points = idx[index,]
  switch(method,
         "DFT-CF" = {
           res = pmd(pp)
           temp.index = idx0[index,]
           prob  = 0
           res.expr="prob = prob + res[temp[1]"
           if(mm>=3)
           {
             for(i in 2:(mm-1))
             {
               res.expr=paste0(res.expr, ", temp[", i, "]")
             }
           }
           res.expr=paste0(res.expr, "]")
           
           for(i in 1:nrow(temp.index)){
            temp =  as.numeric(temp.index[i,])
            eval(parse(text=res.expr))
           }
           
         },
         "simulation" = {
           T=t
           points.pos = points[which(points[,mm]>=0),]
           prob = 0
           for(i in 1:nrow(points.pos)){
             prob = prob + pmd.by.demands(as.numeric(points.pos[i,]),pp,T)
           }
         },
         "NA" = {
           prob = 0
           for(i in 1:nrow(points.pos)){
             prob = prob + pmd(pp,method="NA",vec = points.pos[i,])
           }
         })
  return(prob)
}


###############################################################################
#'pmd.by.demands
#'
#' @param pp The pp is a probability matrix which will be input by user
#' @param x_vec result vec input by user
#' @param t simulation repeat time
#' @return The probability mass of PMD
#' @export
#'
pmd.by.demands = function(x_vec,pp,t=1000){
    x_vec = as.vector(x_vec)
    nn = nrow(pp)
    mm = ncol(pp)
    # if(sum(x_vec)!=nn)   stop("invalid x_vec.")
    res0=0
    temp = .C("pmd_simulation_vec",as.double(res0), as.integer(nn), as.integer(mm), as.double(pp), as.integer(x_vec), as.double(t), PACKAGE = "poissonmulti")
    res0=round(temp[[1]],10)
    return(res0)
}


################################################################################
#' Til
#'
#' @param n column dimension
#' @param m row dimension
#'
#' @return a randomly generated Poisson multinomial distribution probability matrix
#' @export
#'
#' @examples
#' pp = pmatrix(2,2)
#' pp
#' @export
#'
pmatrix <- function(n,m){
  p <- matrix(0,nrow = n,ncol = m,byrow = T)
  for (i in 1:n) {
    r <- runif(m)
    r <- r/sum(r) #generate row
    p[i,] <- r
  }
  return(p)
}




###################################################################################

#' avg.tae.normal
#'
#' @param kk repeat number
#' @param n nrow
#' @param m ncol
#' @return averaged tae
#' @export
#'


avg.tae.normal = function(kk,n,m){
  mm = m
  nn = n
  tae = 0
  nn.vec=rep(nn+1, mm-1)
  l.vec=rep(0, mm-1)
  cn.vec=cumprod(nn.vec)
  cn.vec=c(1, cn.vec[-(mm-1)])
  cn.vec=cn.vec[length(cn.vec):1]
  cn.vec=as.integer(cn.vec)
  nnt=prod(nn.vec)
  for(i in 1:kk){
    pp = pmatrix(n,m)
    res0 = pmd(pp)
    res_normal=double(nnt)
    for(ii in 1:nnt)
    {
      idx=l.vec.compute(k=ii, cn.vec=cn.vec, m=mm)
      #print(idx)
      if(nn-sum(idx-1)<0)
        res_normal[ii] = 0
      else
      {
        vec = c(idx-1,nn-sum(idx-1))
        res_normal[ii] = pmd(pp,method = 'NA',vec)
      }
    }
    ac = abs(res0 - res_normal)
    #if(res1 < ac)
    tae = sum(ac) + tae
  }
  tae = tae/kk
  return(tae)
}

#############################################################################################

#' avg.tae.simu
#'
#' @param kk repeat number
#' @param n nrow
#' @param m ncol
#' @param T repeating time
#' @return averaged tae
#' @export
#'


avg.tae.simu = function(kk,n,m,T){
  tae = 0
  for(i in 1:kk){
    pp = pmatrix(n,m)
    res0 = pmd(pp)
    res_simu=pmd(pp,method='simulation',t=T)
    ac = abs(res0 - res_simu)
    #if(res1 < ac)
    tae = sum(ac) + tae
  }
  tae = tae/kk
  return(tae)
}
################################################################################################################################

