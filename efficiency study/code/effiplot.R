library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(gtable)
library(ggpubr)

m2 <- read.table('effi_m2.txt')
m3 <- read.table('effi_m3.txt')
m4 <- read.table('effi_m4.txt')
m5 <- read.table('effi_m5.txt')



p2 <- m2 %>% ggplot() + geom_path(aes(x=n,y=time)) + labs(title = 'm=2') + 
  scale_y_continuous(breaks = seq(0,0.27,0.03)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('time(s)')
p2


p3 <- m3 %>% ggplot() + geom_path(aes(x=n,y=time)) + 
  labs(title = 'm=3')  + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('time(s)')
p3


p4 <- m4 %>% ggplot() + geom_path(aes(x=n,y=time)) + labs(title = 'm=4') +
  scale_y_continuous(breaks = seq(0,16,2)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab('time(s)')
p4


p5 <- m5 %>% ggplot() + geom_path(aes(x=n,y=time)) + ggtitle('m=5') + 
  ylab('time(s)') +
  scale_y_continuous(breaks = seq(0,100,10)) + 
  theme(plot.title = element_text(hjust = 0.5))
p5


ggarrange(p2, p3, p4, p5, ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
