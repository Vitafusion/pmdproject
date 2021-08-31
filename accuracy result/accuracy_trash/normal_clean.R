t1 = read.table("1.out")
t2 = read.table("2.out")
t3 = read.table("3.out")
t4 = read.table("4.out")
t5 = read.table("5.out")

t1$V3 = t1$V3 + t2$V3 + t3$V3 + t4$V3 + t5$V3
t1$V3 = t1$V3/5
t1 = t1[,-1]
colnames(t1) = c('n','tae')

m3mae = read.table("m3mae.txt")
m3mae = m3mae[c(10,20,30,40,50,60,70,80,90,100),]
m3mae = m3mae[,-1]

m3 = t1
m3$mae = m3mae$V3

m3$'m'=3


m4mae = read.table('m=4.txt')
m4mae = m4mae[,-1]
colnames(m4mae) = c('n','mae')
t1 = read.table("1.out")
t2 = read.table("2.out")
t3 = read.table("3.out")
t4 = read.table("4.out")
t5 = read.table("5.out")
t1$V3 = t1$V3 + t2$V3 + t3$V3 + t4$V3 + t5$V3
t1$V3 = t1$V3/5
t1 = t1[,-1]
colnames(t1) = c('n','tae')
m4 = t1
m4$'mae' = m4mae$mae
m4$'m' = 4



m5 = read.table('m=5_normal.txt')
m5 = m5[,-1]
colnames(m5) = c('n','mae','tae')
m5 = m5[,c('n','tae','mae')]
m5$'m' = 5


m6 = read.table('m=6_normal.txt')
m6 = m6[,-1]
colnames(m6) = c('n','mae','tae')
m6 = m6[,c('n','tae','mae')]
m6$'m' = 6

m7 = read.table('m=7_normal.txt')
m7 = m7[,-1]
colnames(m7) = c('n','mae','tae')
m7 = m7[,c('n','tae','mae')]
m7$'m' = 7


normal_test = rbind(m3,m4,m5,m6,m7)
write.csv(normal_test,'normal_test.csv')
