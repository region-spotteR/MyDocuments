---
author: "Moritz Baten"
date: "August 19, 2018"
title: "SpiRadar: Benchmarks"
output:
  html_document:
    code_folding: hide
    keep_md: true
---


## Overview

About a year ago I was asked to do some visualisation using a spider plot. It had to be interactive and was supposed to compare multiple data points in one plot. Therefore `ggplot2`-based radars like in '@fussballradars' wasn't an option. It had to be `plotly`.

At this time `plotly` didn't offer any default plot type for this. In that way I stumbled upon the `ggradar` package, which built radar or spider plots using `ggplot2`. Since I couldn't use the functions one-to-one in my project, I ran the whole source code from the `ggradar`-function line by line to understand it. I soon realised that I had to rewrite the `ggradar`-function for my own project.

Additionally it turned out that the `ggradar`-function relied on three simple functions, who are used to create the radar plot in cartesian space. Meaning that these functions use sinus and cosinus calculus to transform 'linear' data points into 'circular' space. 

## Set up sample data and parameters

To compare the functions I use the last four rows of the `mtcars`-dataset. I rescale the data to be between zero and one, then I select the last four car models:


```r
rm(list=ls())
my_matrix=as.matrix(mtcars)
my_units<-c("Ford Pantera L","Ferrari Dino","Maserati Bora","Volvo 142E") 
scaled_mat<-apply(my_matrix,2,function(x) (x-min(x,na.rm=TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))  
mat_radar<-scaled_mat[match(my_units,rownames(scaled_mat)),]
round(mat_radar,2)
```

```
##                 mpg cyl disp   hp drat   wt qsec vs am gear carb
## Ford Pantera L 0.23 1.0 0.70 0.75 0.67 0.42 0.00  0  1  1.0 0.43
## Ferrari Dino   0.40 0.5 0.18 0.43 0.40 0.32 0.12  0  1  1.0 0.71
## Maserati Bora  0.20 1.0 0.57 1.00 0.36 0.53 0.01  0  1  1.0 1.00
## Volvo 142E     0.47 0.0 0.12 0.20 0.62 0.32 0.49  1  1  0.5 0.14
```
Please note that I discussed in this post (link will come) the difference between simply rescaling the data or calculating the quantile moment. 

In addition I have to specify some settings for the plot. Their meaning is mostly straightforward:

```r
center.offset=1/9 ## If I set the center of the radar plot to zero I get problems with overplotting (all minimum values will be at (0,0)). Therefore it is wise to move the minium line away from the center. I use 1/9 by default. 
grid.max=1  ## how far shall the 100% line be from the center 
axis.label.offset=1.15 ## If bigger than one, then the axis.label will be further out than the maximum line. Default is 1.15
n_grid=4 ## how many gridlines without the 0% line. Grid vector is created using seq(0,1,1/n). Default is 4 to create a 25%, 50%, 75% and 100% description. Please note that the interpretation varies, depending on whether you calculate quantile moments or just rescale between zero and one.  
r=seq(0,1,1/n_grid) ## radius for all grid lines
```


## Calculating outer grid line of the radar/spider plot

The first function I started to work on was based on the idea of a Stackoverflow user. The idea here (as well as in all the following functions) is to draw a circle. A minimal radar plot consists of two circles: An inner circle where all the minimum data points lie and an outer circle where all the outer datapoints lie. The `ggradar`-package uses the `funcCircleCoords`-function to draw each of these circles. 

The function draws a polygon with multiple evenly spaced 'corners'. The `center`-argument gives where in the cartesian coordinate sytsem the center of the polygon lies. Then the `r`-argument indicates the radius, meaning what distance the polygon has from `center`. Now if we set the `npoints` argument to a very high number (for me anything over 100 does it), it looks as if we have circle. Even though in reality we have a polygon with more than 100 evenly spaced 'corners'. 

First the function creates a sequence of `npoints` evenly spaced between zero and `2*pi`(~6,28). Then the *y-axis* of all polygon points is calculated by multiplying the radius with the *cosinus* of the sequence we created before. The same is done for the *x-axis*, but here we wrap the sequence into a *sinus*-function. Then we give out the result as a dataframe. 


Since this function is only doing the operation for a single radius value, I tested whether you even need the function. Therefore I compared an implementation of the function (`myfun1`) with the way I would calculate the circle (`myfun2`). 


```r
funcCircleCoords <- function(center = center.offset, r = 1, npoints = ncol(mat_radar)){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  yy <- center + r * cos(tt)
  xx <- center + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

axis=NULL # empty list for all axis elements

myfun1<-function(){
  axis$label <-funcCircleCoords(0,(grid.max+abs(center.offset))*axis.label.offset,ncol(mat_radar)+1)[-ncol(mat_radar)-1,]
  axis$label$text=colnames(mat_radar)
}

myfun2<-function(){
  n=ncol(mat_radar)
  angles = seq(from=0, to=((n-1)/n)*2*pi, by=(2*pi)/n)
  xx <- ((grid.max+abs(center.offset))*axis.label.offset)*sin(angles)
  yy <- ((grid.max+abs(center.offset))*axis.label.offset)*cos(angles)
  axis$label<-matrix(c(xx,yy),n,2,dimnames=list(colnames(mat_radar),c('x','y')))
}
```

As a benchmark I calculate the positions for the axis labels. In `myfun1` I use the function from `ggradar` to calculate it. In `myfun2` I use my own approach. Finally I compare both approaches by calling:


```r
microbenchmark::microbenchmark(myfun1(),myfun2(),times=1000L)
```

```
## Unit: microseconds
##      expr     min      lq      mean   median       uq     max neval
##  myfun1() 219.000 239.801 329.52940 273.7515 335.4510 14794.5  1000
##  myfun2()  24.201  32.800  65.35001  41.3005  53.7005 14953.5  1000
```

My approach is a lot faster. This might be because the `ggradar`-function uses dataframes in the output, whereas I use matrices. In addition the `ggradar`-version is not fully adapted to my problem. With adapt I mean that for the application on a radar plot it doesn't matter where the center is. Therefore I removed the center option. For convenience and easier understanding I am setting the center at (0,0) in the cartesic coordinate system.

With these updates of `funcCircleCoords` the approaches should have the same speed:


```r
funcCircleCoords <- function(r = 1, npoints = ncol(plot.data.offset),circle=TRUE){
  if(circle){
    tt <- c(seq(0,((npoints-1)/npoints)*2*pi,length.out = npoints),0)
  } else{
    tt <- seq(0,((npoints-1)/npoints)*2*pi,length.out = npoints)
  }
  yy <- r * cos(tt)
  xx <- r * sin(tt)
  return(matrix(c(xx,yy),length(tt),2,dimnames = list(NULL,c('x','y'))))
}

myfun1<-function(){
  axis$label <-funcCircleCoords((grid.max+abs(center.offset))*axis.label.offset,npoints = ncol(mat_radar),circle = FALSE)
  rownames(axis$label)=colnames(mat_radar)
}

microbenchmark::microbenchmark(myfun1(),myfun2(),times=1000L)
```

```
## Unit: microseconds
##      expr  min     lq     mean median      uq       max neval
##  myfun1() 19.9 21.901 54.20769 22.801 26.5515 24089.301  1000
##  myfun2() 23.4 25.801 32.05731 26.801 29.4500   269.301  1000
```

```r
microbenchmark::microbenchmark(myfun2(),myfun1(),times=1000L)
```

```
## Unit: microseconds
##      expr    min     lq     mean  median     uq     max neval
##  myfun2() 23.301 24.501 29.82999 25.1010 26.901 269.901  1000
##  myfun1() 19.601 20.801 26.44000 21.4015 22.951 206.701  1000
```

Now the function is slightly faster than handwritten code. Therefore I would only use the function if I need to calculate this stuff more than once.

## Transform data points to compare into 'circular' space

The problem of representing different datapoints on radar plot is quite similar to the one discussed before. If you draw a circle you have one radius for each circle you draw, now if you want to represent different datapoints on a radar plot you first need to know where on the circumference you need your datapoints. 

A radar/spider plot usually has straight lines from the center towards the end of the radar. In the `ggradar`-package they were called 'axis'. You have one of these 'axis' for each category you want to use for comparison, i.e., the categories `hp`,`mpg` and 'cyl' in the `mtcars` data. All the different 'axis' have the same distance to each other. 

Since all 'axis' need to have the same distance to each other you now that you will need as many 'axis' as categories. So where on the circumference are now the datapoints? Good old $\pi$ will help you. The circumference of a circle is $2\pi$, hence if we have a sequence of $n$ steps starting at $0$ and ending at $2\pi$, we go full circle (because $\sin(0)=\sin(2\pi)$ and $\cos(0)=\cos(2\pi)$). Therefore to reduce overplotting we need to stop shortly before $2\pi$ 

Thus our sequence needs to stop at some $t<2\pi$. The optimal $t$ is such that all column name are plotted with even distance to each other. It turn out that we can calculate $t$ using a constant $k$ such that $t=k*2*\pi<2*pi$ (meaning $\Rightarrow k<1$). This way we end up with $k=\frac{n-1}{n}, where $n$ is total count of all columns used for comparison. 

Does that guessed solution work? If $k=1$ then $sin(2*pi*1)=sin(0)$ and we get overplotting. We need to divide by $n$, since that secures even spacing of the ploted column names and we need $n-1$ in the nominator, because our first column name gets plotted at $0$.

With this spacing our datapoints will always stay on the same 'axis' in the circumference. But how do we set their 'height'? It turns out that we don't need to take care of that explicitly: To speak in the language of the `funcCircleCoords` each datapoint has a different radius value (because we rescale each datapoint between zero and one).

This similarity between `funcCircleCoords` and `CalculateGroupPath` is what distinguishes my package so much from `ggradar`. My rewritten `CalculateGroupPath`-function looks like this:


```r
CalculateGroupPath6 <- function(mat,circle=TRUE){
  n=ncol(mat)
  if(circle){
    angles = c(seq(from=0, to=((n-1)/n)*2*pi, by=(2*pi)/n),0) # find increment
    mat=mat[,c(1:n,1)]
    group=rep(rownames(mat),each=n+1)
  } else {
      angles = seq(from=0, to=((n-1)/n)*2*pi, by=(2*pi)/n) # find increment
      group=rep(rownames(mat),each=n)
    }
  xx<-c(t(mat)*sin(angles))
  yy<-c(t(mat)*cos(angles))
  graphData<-matrix(c(xx,yy),length(xx),2,dimnames = list(group,c('x','y')))
  return(graphData)
}
```

The math works *exactly* like in `funcCircleCoords`, except that it takes a `matrix` as input instead of a single number `r`. The other difference is that this functions returns the rownames as group variable. 

If these functions are so similar why use `funcCircleCoords`? Firstly because the first version of `CalculateGroupPath` was written in a very general way (general in the sense that this function would work in a lot of programming languages). Unfortunately `R` is not structured like most programming languages, meaning in this case that the first version of `CalculateGroupPath` runs very very slow. I step by step removed the layers which made the function run slow, resulting in the function `CalculuteGroupPath6` shown above. The other functions are appended and benchmarked at the end of this subsection. 

Secondly `CalculateGroupPath` was written to calculate multiple 'circles', meaning that for one circle `funcCircleCoords` will always be faster. That begs the question how it is for two 'circles'?

In the `ggradar`-package, there was a function for this as well - called `CalculateAxisPath`. It basically operates the same way as the other functions and was used to calculate the straight 'axis' lines that start in the center and go towards the end of the radar plane.

Here i managed to speed up the old `ggradar`-function by wrapping some core functionalities into a `lapply`-loop.
The function takes all column names to compare, the minimum grid line (where the minima data points are) and the maximum grid line (maxima data points).

The question now is which of the three functions in the fastest in calculating these two 'circles'? Which will be the fastest function
- `CalculateAxisPath`
- `funcCircleCoords` or the last version of 
- `CalculateGroupPath`?


```r
CalculateAxisPath <- function(var.names,min,max) {
  n<-length(var.names)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  tmp<-lapply(1:n,function(i) matrix(c(i,i,min.x[i],max.x[i],min.y[i],max.y[i]),2,3))
  res<-do.call(rbind,tmp)
  return(res)
}


grid=NULL
myfun1<-function(){
  grid$axis_path  <- CalculateAxisPath(colnames(mat_radar),0+abs(center.offset),grid.max+abs(center.offset))
}

myfun2<-function(){
  n=ncol(mat_radar)
  mymat=matrix(c(0,grid.max)+abs(center.offset),2,n,dimnames = list(c('min','max'),1:n))
 grid$axis_path<-CalculateGroupPath6(mymat,circle = FALSE)
 rownames(grid$axis_path)=rep(1:n, 2)
}

myfun3<-function(){
 grid$path<- lapply(c(0,grid.max)+abs(center.offset),function(x) funcCircleCoords(x,ncol(mat_radar),circle = FALSE))
 grid$axis_path<-do.call(rbind,grid$path)
 n=ncol(mat_radar)
 rownames(grid$axis_path)<-rep(1:n, 2)  # The quantiles of each grid
}
microbenchmark::microbenchmark(myfun1(),myfun2(),myfun3(),times=10000L)
```

```
## Unit: microseconds
##      expr    min     lq     mean median     uq       max neval
##  myfun1() 54.701 60.251 83.51938 63.201 76.701 14772.302 10000
##  myfun2() 43.601 47.602 65.90615 49.902 62.501 24076.900 10000
##  myfun3() 43.801 48.501 64.55663 51.001 63.301  6748.601 10000
```

The slowest function is `CalculateAxisPath`, while `CalculateGroupPath6` and `funcCircleCoords` are on par. However both do the precisely same thing and since it doesn't really make sense to use three very similar functions for the same stuff, I will use `CalculateGroupPath6` as the only helper function in my new update. Mainly because it is the most flexible function of the three and doesn't need any ugly `lapply`-loop for speed like `funcCircleCoords`. 

### Some common sense

For a radar plot there are four components to be calculated

- the grid lines 
- the 'axis' paths/lines
- the positions of the labels
- positions of the datapoints to compare

Now the grid lines and the 'axis' paths share points. Basically the 'axis' paths are a subset of the grid lines (they can be equal to the grid lines if there is only a minimum and maximum grid line). Therefore I wanted to check which is faster: Calculating the 'axis' paths or extracting them from the grid lines. 


```r
### Creating the grid lines
n=ncol(mat_radar)
mymat=matrix(r,length(r),n,dimnames = list(paste0(r,'% Range'),1:n))
grid$new_lines<-CalculateGroupPath6(mymat+center.offset)

## A function to extract the 'axis' paths from the existing grid lines 
myfun1<-function(){ 
  n=dim(mat_radar)[2]
  myrows<-c(1:n,(dim(grid$new_lines)[1]-n):(dim(grid$new_lines)[1]-1))
  xx=grid$new_lines[myrows,'x']
  yy=grid$new_lines[myrows,'y']
  axis.no=rep(1:ncol(mat_radar),2)
  grid$axis_path<-matrix(c(xx,yy),length(xx),dimnames = list(axis.no, c('x','y')))
}

## myfun2() is the function to create the 'axis' paths from the last chunk
microbenchmark::microbenchmark(myfun1(),myfun2(),times=1000L)
```

```
## Unit: microseconds
##      expr    min     lq     mean  median     uq      max neval
##  myfun1()  9.100 10.801 24.65039 11.8005 16.301 8917.801  1000
##  myfun2() 43.701 47.001 70.13586 48.9000 66.151 2909.201  1000
```

```r
microbenchmark::microbenchmark(myfun2(),myfun1(),times=1000L)
```

```
## Unit: microseconds
##      expr    min     lq     mean median      uq     max neval
##  myfun2() 43.101 45.601 56.22463 47.201 58.3015 560.801  1000
##  myfun1()  9.000 10.301 12.73331 11.200 12.3010  59.802  1000
```


# Appendix

## Benchmark of all `CalculateGroupPath`-functions




```r
CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
  
  path <- rownames(df)
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)))
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]
  
  for(i in unique(as.character(path))){
    pathData = subset(df, rownames(df)==i)
    for(j in c(1:ncol(df))){
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j]),
                                            y=pathData[,j]*cos(angles[j])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,1]*sin(angles[1]),
                                          y=pathData[,1]*cos(angles[1])))
  }
  #Make sure that name of first column matches that of input data (in case !="group")
  graphData #data frame returned by function
}

CalculateGroupPath2 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df))) # find increment
  xx<-sapply(1:nrow(df), function (j) c(df[j,]*sin(angles[-ncol(df)-1]),df[j,1]*sin(angles[1])))
  yy<-sapply(1:nrow(df), function (j) c(df[j,]*cos(angles[-ncol(df)-1]),df[j,1]*cos(angles[1])))
  graphData<-data.frame(group=rep(rownames(df),each=ncol(df)+1),x=c(xx),y=c(yy))
  return(graphData)
}
CalculateGroupPath3 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df))) # find increment
  xx<-lapply(1:nrow(df), function (j) c(df[j,]*sin(angles[-ncol(df)-1]),df[j,1]*sin(angles[1])))
  yy<-lapply(1:nrow(df), function (j) c(df[j,]*cos(angles[-ncol(df)-1]),df[j,1]*cos(angles[1])))
  graphData<-data.frame(group=rep(rownames(df),each=ncol(df)+1),x=unlist(xx),y=unlist(yy))
  return(graphData)
}
CalculateGroupPath4 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df))) # find increment
  xx<-c(rbind(t(df)*sin(angles[-ncol(df)-1]),t(df[,1])*sin(angles[1])))
  yy<-c(rbind(t(df)*cos(angles[-ncol(df)-1]),t(df[,1])*cos(angles[1])))
  graphData<-data.frame(group=rep(rownames(df),each=ncol(df)+1),x=(xx),y=(yy))
  return(graphData)
}
CalculateGroupPath5 <- function(mydf) {
  df<-cbind(mydf,mydf[,1])
  myvec<-c(t(df))
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-myvec*sin(rep(c(angles[-ncol(df)],angles[1]),nrow(df)))
  yy<-myvec*cos(rep(c(angles[-ncol(df)],angles[1]),nrow(df)))
  graphData<-data.frame(group=rep(rownames(mydf),each=ncol(mydf)+1),x=(xx),y=(yy))
  return(graphData)
}
CalculateGroupPath6 <- function(mat,circle=TRUE){
  n=ncol(mat)
  if(circle){
    angles = c(seq(from=0, to=((n-1)/n)*2*pi, by=(2*pi)/n),0) # find increment
    mat=mat[,c(1:n,1)]
    group=rep(rownames(mat),each=n+1)
  } else {
      angles = seq(from=0, to=((n-1)/n)*2*pi, by=(2*pi)/n) # find increment
      group=rep(rownames(mat),each=n)
    }
  xx<-c(t(mat)*sin(angles))
  yy<-c(t(mat)*cos(angles))
  graphData<-matrix(c(xx,yy),length(xx),2,dimnames = list(group,c('x','y')))
  return(graphData)
}

microbenchmark::microbenchmark(CalculateGroupPath(mymat),CalculateGroupPath2(mymat),
                               CalculateGroupPath3(mymat),CalculateGroupPath4(mymat),
                               CalculateGroupPath5(mymat),CalculateGroupPath6(mymat),times = 1000L)
```

```
## Unit: microseconds
##                        expr       min         lq       mean     median
##   CalculateGroupPath(mymat) 30417.901 32463.4510 36028.6530 34152.3510
##  CalculateGroupPath2(mymat)   397.000   444.4015   569.3920   482.0010
##  CalculateGroupPath3(mymat)   364.701   406.7505   539.3184   443.4005
##  CalculateGroupPath4(mymat)   314.801   353.2510   469.0601   391.7510
##  CalculateGroupPath5(mymat)   304.002   334.8010   437.3525   369.4005
##  CalculateGroupPath6(mymat)    40.401    63.3505   104.6473    73.5010
##          uq      max neval
##  36406.5510 115651.5  1000
##    541.4010  16942.4  1000
##    511.5015  17762.5  1000
##    450.8510  23050.4  1000
##    429.4010  10651.4  1000
##     92.3010  22289.8  1000
```

## Calculating the 'axis' lines




```
## Unit: microseconds
##                                                                                                 expr
##   CalculateAxisPath(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
##  CalculateAxisPath2(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
##  CalculateAxisPath3(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
##  CalculateAxisPath4(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
##      min      lq     mean   median       uq       max neval
##   97.100 120.051 205.9199 151.9015 223.5010 22184.801  1000
##   79.901 102.200 168.4696 130.0510 189.0010 12884.201  1000
##  216.701 258.651 398.8442 339.7005 482.3005  7781.101  1000
##   56.201  70.401 120.0812  89.7010 130.5510 11477.701  1000
```

