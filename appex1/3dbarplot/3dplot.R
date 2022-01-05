##########################################################
# Generating Result Plots For Voting Probability Example #
##########################################################


# load p matrix
load("Sec5.1/data/pmd.RData")

# load Poisson multinomial result data
load("Sec5.1/data/res.RData")


library('plot.matrix')
library(plot3D)

# build a dataframe to contain the result computed from dpmd
y <- x <- c(1:11)

z <- matrix(0, nrow=11, ncol=11)
result.mat <- matrix('tie', nrow=11, ncol=11)
for(i in 1:nrow(z)){
  for (j in 1:ncol(z)) {
    z[i,j] <- res[i,j]
    x1 <- i-1
    x2 <- j-1
    x3 <- 10-x1-x2
    if(x1>x2 && x1>x3)
      result.mat[i,j]='x1'
    if(x2>x1 && x2>x3)
      result.mat[i,j]='x2'
    if(x3>x2 && x3>x1)
      result.mat[i,j]='x3'
  }
}



mat1=result.mat
mat1[mat1=="x1"]="C1"
mat1[mat1=="x2"]="C2"
mat1[mat1=="x3"]="C3"
mat1[mat1=="tie"]="Tie"

for(i in 1:11)
{
  for(j in 1:11)
  {
    if(i+j>12)
    {
      mat1[i,j]=" "
    }
  }
}


# 2D plot
par(mar=c(2,4,4,2))
plot(mat1, digits=3, 
     col=c("white","red","grey","green","yellow"), 
     text.cell=list(pos=3, cex=0.9, offset=-0.1), 
     xlab="", ylab="", cex.lab=1,
     key=NULL, main="", axis.col=list(side=3, las=1), axis.row = list(side=2, las=2))

mtext(text = expression(x[2]),
      side = 3,#side 1 = bottom
      line = 2.5, cex=1.5)
mtext(text = expression(x[1]),
      side = 2,#side 2 = left
      line = 2.5, cex=1.5)






z1=z
z1[z1==0]=NA

# 3dbarplot
par(mar=c(0,0.3,1,0))
hist3D(z = z1, x = x, y = y, col = "lightblue",
       border = "black", lighting = T, phi=20, theta = 60, space=0.0,
       cex.lab=1, 
       xlab="", 
       ylab="",
       zlab="")
text(-0.385, 0.065, "Probability", cex=1.3, srt=-82)
text(-0.278, -0.340, expression(x[1]), cex=1.5, srt=-65)
text(0.175, -0.425, expression(x[2]), cex=1.5, srt=30)

