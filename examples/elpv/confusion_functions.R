



confusion_df = function(pmat, type){
  
  pmat_A = as.matrix(pmat[ytrue==type,])
  Asam = rpmd(pmat_A, s = 10000)
  colnames(Asam) = paste0("Pred:",c("1","2","3","4"))
  Asam = as.data.frame(Asam)
  Asam = Asam %>% gather(predclass, n)
  Asam$trueclass = paste0("True:",type)
  Asam$expect = numeric(nrow(Asam))
  Asam$expect[Asam$predclass == paste0("Pred:",type)] = type_count$n[type_count$V5 == type]
  return(Asam)
  
}




marginal_bar = function(pmat, type){
  pmat_B = as.matrix(pmat[ytrue==type,])
  dB = dpmd(pmat_B)
  dba = dbb = dbc = dbd = numeric(type_count$n[type_count$V5==type]+1) # legnth of n+1
  for(i in 1:length(dba)){
    print(i)
    dba[i] = sum(dB[(i),,])
    dbb[i] = sum(dB[,i,])
    dbc[i] = sum(dB[,,i])
  }
  # since the D class can not obtained in the above way, we switch the column 
  # between A and D. Let D to be the first column to let the compute easier
  pmat_Bprime = pmat_B
  pmat_Bprime[,1] = pmat_B[,4]
  pmat_Bprime[,4] = pmat_B[,1]
  dBprime = dpmd(pmat_Bprime)
  for(i in 1:length(dbd)){
    print(i)
    dbd[i] = sum(dBprime[(i),,])
  }
  count = as.factor(0:type_count$n[type_count$V5==type])
  outputdf = cbind(count,dba,dbb,dbc,dbd)
  colnames(outputdf) = c("count",paste0("Pred:",c("1","2","3","4")))
  outputdf= as.data.frame(outputdf)
  outputdf = outputdf %>% gather(predclass, n,-count)
  outputdf$trueclass = paste0("True:",type)
  return(outputdf)
}