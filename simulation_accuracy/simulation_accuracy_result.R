setwd("C:/Users/linzh/Desktop/pmd/simulation_accuracy")

m3 = read.table('m3.txt',sep = ',')
m3 = m3[,-1]
m3 = as.data.frame(m3)
colnames(m3) = c('n','(p_min,p_max)','MAE','TAE')

m4 = read.table('m4.txt')
m4 = m4[,-1]
m4 = as.data.frame(m4)
colnames(m4) = c('n','(p_min,p_max)','MAE','TAE')







par(mfrow = c(2, 2))
plot(x = m3$n,y=m3$MAE,type = 'l')
plot(x = m3$n,y=m3$TAE,type = 'l')

plot(x = m4$n,y=m4$MAE,type = 'l')
plot(x = m4$n,y=m4$TAE,type = 'l')



