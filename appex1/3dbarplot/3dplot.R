load("res.RData")


#build a dataframe to contain the result computed from dpmd
y <- z <- x <- c() 

for(i in 1:11){
  for(j in 1:11){
    x <- c(x,i)
    y <- c(y,j)
    z <- c(z,res[i,j])
  }
}

#build a dataframe to contain the results
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



##2D matrix plot
library('plot.matrix')
y <- x <- c(1:11)

z <- matrix(0, nrow=11, ncol=11)
result.mat <- matrix('tie', nrow=11, ncol=11)
for(i in 1:nrow(z)){
  for (j in 1:ncol(z)) {
    z[i,j] <- res1[i,j]
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


plot(result.mat, digits=3, 
     col=c("red","grey","green","yellow"), 
     text.cell=list(pos=3, cex=0.9, offset=-0.1), 
     xlab="", ylab="", cex.lab=1,
     key=NULL)
mtext(text = "x1",
      side = 1,#side 1 = bottom
      line = 2, cex=1.2)
mtext(text = "x2",
      side = 2,#side 2 = left
      line = 2, cex=1.2)


##3dbarplot
library(plot3D)
hist3D(z = z, x = x, y = y, col = "White",
       border = "black", lighting = T, phi=40, theta = 40,space=0.0,
       cex.lab=0.8, 
       xlab="x1", 
       ylab="x2",
       zlab="prob")




##the plot java script

// prettier-ignore
var data_green = [[1, 1, 2.4779e-06],
 [1, 2, 0.0001935626],
 [1, 3, 0.0015438042],
 [1, 4, 0.0053884259],
 [2, 1, 8.96971e-05],
 [2, 2, 0.0019149687],
 [2, 3, 0.0110845449],
 [3, 1, 0.0005419179],
 [3, 2, 0.0071219765],
 [3, 3, 0.0314152569],
 [4, 1, 0.0014393017]];

var data_red = [[1, 5, 0.0105143021],
 [1, 6, 0.0125847792],
 [1, 7, 0.0095605404],
 [1, 8, 0.0045924964],
 [1, 9, 0.0013346287],
 [1, 10, 0.0002094852],
 [1, 11, 1.30423e-05],
 [2, 5, 0.0484915185],
 [2, 6, 0.0467303218],
 [2, 7, 0.0279257028],
 [2, 8, 0.0100592127],
 [2, 9, 0.0019874019],
 [2, 10, 0.0001637268],
 [2, 11, 0.0],
 [3, 4, 0.0689883686],
 [3, 5, 0.0863571483],
 [3, 6, 0.0645092939],
 [3, 7, 0.0284366145],
 [3, 8, 0.0068152028],
 [3, 9, 0.000683154],
 [3, 10, 0.0],
 [3, 11, 0.0],
 [4, 5, 0.0750050521],
 [4, 6, 0.04070328],
 [4, 7, 0.0117188791],
 [4, 8, 0.0013900796],
 [4, 9, 0.0],
 [4, 10, 0.0],
 [4, 11, 0.0],
 [5, 6, 0.0115028316],
 [5, 7, 0.0016112552],
 [5, 8, 0.0],
 [5, 9, 0.0],
 [5, 10, 0.0],
 [5, 11, 0.0],
 [6, 7, 0.0],
 [6, 8, 0.0],
 [6, 9, 0.0],
 [6, 10, 0.0],
 [6, 11, 0.0],
 [7, 8, 0.0],
 [7, 9, 0.0],
 [7, 10, 0.0],
 [7, 11, 0.0],
 [8, 9, 0.0],
 [8, 10, 0.0],
 [8, 11, 0.0],
 [9, 10, 0.0],
 [9, 11, 0.0],
 [10, 11, 0.0]];

var data_blue = [[4, 3, 0.0458231014],
 [5, 1, 0.0020606479],
 [5, 2, 0.0143384947],
 [5, 3, 0.0373061391],
 [5, 4, 0.0480728091],
 [6, 1, 0.0017052875],
 [6, 2, 0.0088787736],
 [6, 3, 0.0171422234],
 [6, 4, 0.0156343584],
 [6, 5, 0.0068121285],
 [7, 1, 0.000830133],
 [7, 2, 0.0031690191],
 [7, 3, 0.0042868558],
 [7, 4, 0.0024622764],
 [7, 5, 0.0005105737],
 [7, 6, 0.0],
 [8, 1, 0.0002351827],
 [8, 2, 0.0006240064],
 [8, 3, 0.000527223],
 [8, 4, 0.0001428653],
 [8, 5, 0.0],
 [8, 6, 0.0],
 [8, 7, 0.0],
 [9, 1, 3.69923e-05],
 [9, 2, 6.0819e-05],
 [9, 3, 2.40054e-05],
 [9, 4, 0.0],
 [9, 5, 0.0],
 [9, 6, 0.0],
 [9, 7, 0.0],
 [9, 8, 0.0],
 [10, 1, 2.8734e-06],
 [10, 2, 2.1747e-06],
 [10, 3, 0.0],
 [10, 4, 0.0],
 [10, 5, 0.0],
 [10, 6, 0.0],
 [10, 7, 0.0],
 [10, 8, 0.0],
 [10, 9, 0.0],
 [11, 1, 7.92e-08],
 [11, 2, 0.0],
 [11, 3, 0.0],
 [11, 4, 0.0],
 [11, 5, 0.0],
 [11, 6, 0.0],
 [11, 7, 0.0],
 [11, 8, 0.0],
 [11, 9, 0.0],
 [11, 10, 0.0]];

var data_yellow = [[2, 4, 0.0307105992],
 [4, 2, 0.0134631392],
 [4, 4, 0.078430491],
 [5, 5, 0.0329393232],
 [6, 6, 0.0011431514],
 [7, 7, 0.0],
 [8, 8, 0.0],
 [9, 9, 0.0],
 [10, 10, 0.0],
 [11, 11, 0.0]];
option = {
  tooltip: {},
  xAxis3D: {
    type: 'value',
  },
  yAxis3D: {
    type: 'value',
  },
  zAxis3D: {
    type: 'value'
  },
  grid3D: {
    boxWidth: 80,
    boxDepth: 80,
    viewControl: {
      // projection: 'orthographic'
    },
    light: {
      main: {
        intensity: 1.2,
        shadow: true
      },
      ambient: {
        intensity: 0.3
      }
    }
  },
  series: [
    {
      type: 'bar3D',
      data: data_green.map(function (item) {
        return {
          value: [item[1], item[0], item[2]]
        };
      }),
      shading: 'lambert',
      color: 'green',
      label: {
        fontSize: 16,
        borderWidth: 1
      },
      emphasis: {
        label: {
          fontSize: 20,
          color: '#000'
        },
        itemStyle: {
          color: '#900'
        }
      }
    },{
      type: 'bar3D',
      data: data_blue.map(function (item) {
        return {
          value: [item[1], item[0], item[2]]
        };
      }),
      shading: 'lambert',
      color: 'blue',
      label: {
        fontSize: 16,
        borderWidth: 1
      },
      emphasis: {
        label: {
          fontSize: 20,
          color: '#000'
        },
        itemStyle: {
          color: '#900'
        }
      }
    },{
      type: 'bar3D',
      data: data_red.map(function (item) {
        return {
          value: [item[1], item[0], item[2]]
        };
      }),
      shading: 'lambert',
      color: 'red',
      label: {
        fontSize: 16,
        borderWidth: 1
      },
      emphasis: {
        label: {
          fontSize: 20,
          color: '#000'
        },
        itemStyle: {
          color: '#900'
        }
      }
    },{
      type: 'bar3D',
      data: data_yellow.map(function (item) {
        return {
          value: [item[1], item[0], item[2]]
        };
      }),
      shading: 'lambert',
      color: 'yellow',
      label: {
        fontSize: 16,
        borderWidth: 1
      },
      emphasis: {
        label: {
          fontSize: 20,
          color: '#000'
        },
        itemStyle: {
          color: '#900'
        }
      }
    }
  ]
};
