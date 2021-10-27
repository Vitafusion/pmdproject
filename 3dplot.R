load("res.RData")

y <- z <- x <- c() 

for(i in 1:11){
  for(j in 1:11){
    x <- c(x,i)
    y <- c(y,j)
    z <- c(z,res[i,j])
  }
}

m <- as.data.frame(matrix(c(x,y,z),ncol=3))
colnames(m) <- c("x","y","z")
m$col <- "dd"
m$shape <- "s"
n <- 10
for (i in 1:nrow(m)) {
  k <- n - m$x[i] - m$y[i]
  if(m$x[i] > m$y[i] && m$x[i] > k)
    m$col[i] <- "blue"
  m$shape[i] <- "solid"
  if(m$y[i] > m$x[i] && m$y[i] > k)
    m$col[i] <- "red"
  m$shape[i] <- "dash"
  if(m$y[i] < k && m$x[i] < k)
    m$col[i] <- "green"
  m$shape[i] <- "dash-dash"
  
}


