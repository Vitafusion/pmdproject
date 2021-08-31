#bino plots
setwd("C:/Users/linzh/Desktop/pmd/compa_bino")
source("C:/Users/linzh/Desktop/pmd/functions/myfunctions.R")
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)


bino = load.bino()
p = bino.plot(bino)

pdf(file = 'bino_MAE.pdf')
p[[1]]
dev.off()

pdf(file = 'bino_TAE.pdf')
p[[2]]
dev.off()
