library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)

setwd("~/Desktop/research/pmd/pmdproject/pmdproject/accuracy result/data")

dat = read.table('accura_dat.txt',sep = '\t')
dat$m = as.factor(dat$m)
dat$method = as.factor(dat$method)


dat.simu <- read.table("simuacc.txt",sep = '\t')
dat.simu$B <- as.factor(dat.simu$B)


#mae
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

scaleFUN <- function(x) sprintf("%.4f", x)

p3 = dat %>% filter(m==3) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = T) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = 'm=3') + 
  ylab("MAE") + theme(plot.subtitle = element_text(hjust = 0.5))

p3


base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

scaleFUN <- function(x) sprintf("%.4f", x)

p4 = dat %>% filter(m==4) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks(),labels = scaleFUN) +labs(subtitle = 'm=4') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))

p4



base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}
scaleFUN <- function(x) sprintf("%.4f", x)
p5 = dat %>% filter(m==5) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = scaleFUN) +labs(subtitle = 'm=5') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))

p5

p6 = dat %>% filter(m==6) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),breaks = base_breaks(), labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("MAE") + theme(plot.subtitle = element_text(hjust = 0.5))


p6

p7 = dat %>% filter(m==7) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),breaks = base_breaks(),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))

p7

pdf(file = 'mae_all.pdf')

#grid.arrange(p3,p4,p5,p6,p7,ncol = 3, nrow = 2)
ggarrange(p3,p5,p7, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

dev.off()

#tae


scaleFUN <- function(x) sprintf("%.1f", x)
p3 = dat %>% filter(m==3,
                    method=='SIM'|method=='N.A',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN,breaks = seq(0,1,0.1),limits = c(0.4,0.6)) +labs(subtitle = 'm=3') + 
  ylab("TAE") + theme(plot.subtitle = element_text(hjust = 0.5))

p3

p4 = dat %>% filter(m==4,
                    method=='SIM'|method=='N.A',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=4') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))
p4

scaleFUN <- function(x) sprintf("%.3f", x)
p5 = dat %>% filter(m==5,method=='SIM'|method=='N.A') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = 'log10',breaks = c(1,0.1,0.01,0.001),labels = scaleFUN) +labs(subtitle = 'm=5') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))

p5

scaleFUN <- function(x) sprintf("%.3f", x)
p6 = dat %>% filter(m==6,method=='SIM'|method=='N.A') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = 'log10', breaks = c(1,0.1,0.01,0.001), labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("TAE") + theme(plot.subtitle = element_text(hjust = 0.5))

p6


scaleFUN <- function(x) sprintf("%.3f", x)
p7 = dat %>% filter(m==7,method=='SIM'|method=='N.A') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),breaks = c(1,0.1,0.01,0.001),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank()) + theme(plot.subtitle = element_text(hjust = 0.5))

p7

pdf(file = 'tae_all.pdf')

#grid.arrange(p3,p4,p5,p6,p7,ncol = 3, nrow = 2)

ggarrange(p3,p5,p7, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


dev.off()



# simulation


base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

scaleFUN <- function(x) sprintf("%.4f", x)

p1 <- dat.simu %>% filter(B==10|B==1e+05|B==1e+07) %>% ggplot() + 
  geom_path(aes(x=n,y=err.max,colour=B,group=B,linetype=B), show.legend = T) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = '100% Quantile') + 
  ylab("error") + theme(plot.subtitle = element_text(hjust = 0.5))

p2 <- dat.simu %>% filter(n<40) %>% filter(B==10|B==1e+05|B==1e+07) %>% ggplot() + 
  geom_path(aes(x=n,y=err.95,colour=B,group=B,linetype=B), show.legend = T) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = '95% Quantile') + 
  ylab("error") + theme(plot.subtitle = element_text(hjust = 0.5))


p3 <- dat.simu %>% filter(n<30) %>% filter(B==10|B==1e+05|B==1e+07) %>% ggplot() + 
  geom_path(aes(x=n,y=err.90,colour=B,group=B,linetype=B), show.legend = T) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = '90% Quantile') + 
  ylab("error") + theme(plot.subtitle = element_text(hjust = 0.5))


ggarrange(p1,p2,p3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

