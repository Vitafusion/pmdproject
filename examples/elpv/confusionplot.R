# This file contains R codes for section 5.3 in paper 
# "The Poisson Multinomial Distribution and Its Applications in Voting Theory, Ecological Inference, and Machine Learning"
# by Zhengzhi Lin, Yueyao Wang, and Yili Hong.




dat = read.csv("PMDpmat.csv", header = F)
library(PoissonMultinomial)
library(tidyr)
library(dplyr)
source('confusion_functions.R')

ytrue = vector("character",nrow(dat))
ytrue_before = dat[,5]
for(i in 1:length(ytrue)){
  if(ytrue_before[i] == 0){
    ytrue[i] = "1"
  }else if(ytrue_before[i] == 0.33333){
    ytrue[i] = "2"
  }else if(ytrue_before[i] == 0.66667){
    ytrue[i] = "3"
  }else if(ytrue_before[i] == 1){
    ytrue[i] = "4"
  }
}



pmat = dat[,1:4]
pmat[,4] = 1-rowSums(pmat[,1:3])
datnew = dat
datnew[,5] = ytrue
datdf = as.data.frame(datnew)
type_count = (datdf %>% group_by(V5) %>% summarise(n = n()))





Adf = confusion_df(pmat, "1")
Bdf = confusion_df(pmat, "2")
Cdf = confusion_df(pmat, "3")
Ddf = confusion_df(pmat, "4")

df = rbind(Adf,Bdf,Cdf,Ddf)

library(ggplot2)
ggplot(df, aes(x = n))+ geom_histogram() +
  facet_grid(predclass~ trueclass,scales = 'free_x')


outA = (marginal_bar(pmat, "1"))
outB = (marginal_bar(pmat, "2"))
outC = (marginal_bar(pmat, type = "3"))
outD = (marginal_bar(pmat, "4"))

save(outA, outB,outC,outD,file = "marginal")
#save(outA, file = "marginalA")
out = as.data.frame(rbind(outA, outB, outC, outD))
colnames(out) = c("name","predclass","value","trueclass")
library(ggplot2)
out2 = out[out$value!=0,]
ggplot(data = out2, aes(x =name, y = value))+ geom_bar(stat="identity") +
  #geom_vline(aes(xintercept=expect),color="black", linetype="dashed", size=1) + 
  facet_grid(predclass~ trueclass,scales = 'free_x') +
  expand_limits(x = 0) + scale_x_continuous()
#theme_tufte() + theme(axis.line=element_line())



# 
# layout(matrix(c(1,2,3,4,5,
#                 6,7,8,9,10,
#                 6,7,8,9,10,
#                 rep(seq(11,15,by = 1),2),
#                 rep(seq(16,20,by = 1),2),
#                 rep(seq(21,25,by = 1),2),nrow = 5, byrow = T)))
load("marginal.RData")
out = as.data.frame(rbind(outA, outB, outC, outD))
colnames(out) = c("name","predclass","value","trueclass")
out2 = out[out$value!=0,]
out2$predclass = unlist(lapply(strsplit(out2$predclass,split = ":"), function(x){paste(x[1],x[2])}))
out2$trueclass = unlist(lapply(strsplit(out2$trueclass,split = ":"), function(x){paste(x[1],x[2])}))

trueclass = unique(out2$trueclass)
predclass = unique(out2$predclass)



setEPS()

postscript("Confusionbar.eps")

par(oma = c(5,5,2,1.5), mfrow = c(4,4),mar=c(1,0,1.5,1.5))

for(i in 1:length(predclass)){ 
  for(j in 1:length(trueclass)){
    sub = out2[(out2$trueclass == trueclass[j] & out2$predclass == predclass[i]),]
    sub$name = as.numeric(sub$name) - 1
    ex = sum((as.numeric(sub$name))*sub$value)
    ex2 = sum((as.numeric(sub$name))^2*sub$value)
    var = ex2 - ex^2
    probs = cumsum(sub$value)
    
    
    cil = sub$name[max(which(probs<=0.025))]
    if(is.na(cil)){
      cil = 0
    }
    ciu = sub$name[max(which(probs<=0.975))]
    barplot(value~name, data = sub, ylim = c(0,0.4), axes = F, font.axis= 1, cex.names=1.5)
    corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
    par(xpd = TRUE) #Draw outside plot area
    text(x = mean(corners[1:2]), y = 0.3, paste("mean =", round(ex)), cex=1.4)
    # cil = ex - 1.96*sqrt(var)
    # ciu = ex + 1.96*sqrt(var)
    text(x = mean(corners[1:2])+1.2, y = 0.25, paste("[", round(cil,2), ",",round(ciu,2),"]"), cex=1.3)
    if(j == 1){
      axis(2, cex.axis=1.5)
    }
    if(i == 1){
      mtext(trueclass[j], side = 3,line = 1, font=2, cex=1.3)
    }
    if(j == 4){
      corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
      par(xpd = TRUE) #Draw outside plot area
      text(x = corners[2], y = mean(corners[3:4]), predclass[i], srt = -90, cex = 1.8, font=2)
      #mtext(trueclass[i], side = 4)
    }
    if(j == 1 & i == 2){
      mtext(text = expression(bold("Probability")), side=2, line = 3, adj = -1, cex=1.2)
    }
    if(i == 4 & j == 2){
      mtext(text = expression(bold("Count")),side=1, adj =1.2,line = 3, cex=1.2)
    }
  }
}

dev.off()

