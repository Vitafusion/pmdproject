library(ggplot2)
bench <- read.table('benchres.txt')

p1 <- ggplot(data = bench, aes(x=n)) + geom_line(aes(y=time,color=package,linetype=package))

p1
