library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)

setwd("~/Desktop/research/pmd/pmdproject/pmdproject/accuracy result/data")

dat = read.table('accura_dat.txt')
dat$m = as.factor(dat$m)
dat$method = as.factor(dat$method)


#mae
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

scaleFUN <- function(x) sprintf("%.4f", x)

p3 = dat %>% filter(m==3) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = 'log10', breaks = base_breaks()) +labs(subtitle = 'm=3') + 
  ylab("MAE")

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
  theme(axis.title.y=element_blank())

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
  theme(axis.title.y=element_blank())

p5

p6 = dat %>% filter(m==6) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),breaks = base_breaks(), labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("MAE")

p6

p7 = dat %>% filter(m==7) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),breaks = base_breaks(),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank())

p7

pdf(file = 'mae_all.pdf')

grid.arrange(p3,p4,p5,p6,p7,
             ncol = 3, nrow = 2)

dev.off()

#tae


scaleFUN <- function(x) sprintf("%.1f", x)
p3 = dat %>% filter(m==3,
                    method=='simulation'|method=='normal',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN,breaks = seq(0,1,0.1),limits = c(0.4,0.6)) +labs(subtitle = 'm=3') + 
  ylab("TAE")

p3

p4 = dat %>% filter(m==4,
                    method=='simulation'|method=='normal',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=4') + 
  theme(axis.title.y=element_blank())
p4

scaleFUN <- function(x) sprintf("%.1f", x)
p5 = dat %>% filter(m==5,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=5') + 
  theme(axis.title.y=element_blank())

p5

scaleFUN <- function(x) sprintf("%.2f", x)
p6 = dat %>% filter(m==6,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("TAE")

p6


scaleFUN <- function(x) sprintf("%.3f", x)
p7 = dat %>% filter(m==7,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),breaks = base_breaks(),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank())

p7

pdf(file = 'tae_all.pdf')

grid.arrange(p3,p4,p5,p6,p7,
             ncol = 3, nrow = 2)

dev.off()



