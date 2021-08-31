library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)

dat = read.table('accura_dat.txt')
dat$m = as.factor(dat$m)
dat$method = as.factor(dat$method)


#mae

scaleFUN <- function(x) sprintf("%.4f", x)
p3 = dat %>% filter(m==3) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=3') + 
  ylab("MAE")

p4 = dat %>% filter(m==4) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=4') + 
  theme(axis.title.y=element_blank())


p5 = dat %>% filter(m==5) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=5') + 
  theme(axis.title.y=element_blank())


p6 = dat %>% filter(m==6) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("MAE")


p7 = dat %>% filter(m==7) %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank())

pdf(file = 'mae_all.pdf')

grid.arrange(p3,p4,p5,p6,p7,
             ncol = 3, nrow = 2)

dev.off()

#tae


scaleFUN <- function(x) sprintf("%.4f", x)
p3 = dat %>% filter(m==3,
                    method=='simulation'|method=='normal',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=3') + 
  ylab("TAE")

p4 = dat %>% filter(m==4,
                    method=='simulation'|method=='normal',
                    n==10|n==20|n==30||n==40|n==50|n==60|n==70|n==80|n==90|n==100) %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=4') + 
  theme(axis.title.y=element_blank())


p5 = dat %>% filter(m==5,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=5') + 
  theme(axis.title.y=element_blank())


p6 = dat %>% filter(m==6,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method), show.legend = F) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=6') + 
  ylab("TAE")


p7 = dat %>% filter(m==7,method=='simulation'|method=='normal') %>% ggplot() + 
  geom_path(aes(x=n,y=tae,colour=method,group=method,linetype=method)) + 
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=7') + 
  theme(axis.title.y=element_blank())



pdf(file = 'tae_all.pdf')

grid.arrange(p3,p4,p5,p6,p7,
             ncol = 3, nrow = 2)

dev.off()




dat %>% ggplot() + 
  geom_path(aes(x=n,y=mae,colour=method,linetype=method)) + facet_wrap(~m)
  scale_y_continuous(trans = log_trans(),labels = scaleFUN) +labs(subtitle = 'm=3')
