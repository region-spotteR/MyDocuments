SpiRadar: Benchmarks
================
Moritz Baten
August 19, 2018

Overview
--------

About a year ago I was asked to do some visualisation using a spider plot. It had to be interactive and was supposed to compare multiple data points in one plot. Therefore `ggplot2`-based radars like in '@fussballradars' wasn't an option. It had to be `plotly`.

At this time `plotly` didn't offer any default plot type for this. In that way I stumbled upon the `ggradar` package, which built radar or spider plots using `ggplot2`. Since I couldn't use the functions one-to-one in my project, I ran the whole source code from the `ggradar`-function line by line to understand it. I soon realised that I had to rewrite the `ggradar`-function for my own project.

Additionally it turned out that the `ggradar`-function relied on three simple functions, who are used to create the radar plot in cartesian space. Meaning that these functions use sinus and cosinus calculus to transform 'linear' data points into 'circular' space.

Set up sample data and parameters
---------------------------------

To compare the functions I use the last four rows of the `mtcars`-dataset. I rescale the data to be between zero and one, then I select the last four car models:

``` r
rm(list=ls())
my_matrix=as.matrix(mtcars)
my_units<-c("Ford Pantera L","Ferrari Dino","Maserati Bora","Volvo 142E") 
scaled_mat<-apply(my_matrix,2,function(x) (x-min(x,na.rm=TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))  
mat_radar<-scaled_mat[match(my_units,rownames(scaled_mat)),]
round(mat_radar,2)
```

    ##                 mpg cyl disp   hp drat   wt qsec vs am gear carb
    ## Ford Pantera L 0.23 1.0 0.70 0.75 0.67 0.42 0.00  0  1  1.0 0.43
    ## Ferrari Dino   0.40 0.5 0.18 0.43 0.40 0.32 0.12  0  1  1.0 0.71
    ## Maserati Bora  0.20 1.0 0.57 1.00 0.36 0.53 0.01  0  1  1.0 1.00
    ## Volvo 142E     0.47 0.0 0.12 0.20 0.62 0.32 0.49  1  1  0.5 0.14

Please note that I discussed in this post (link will come) the difference between simply rescaling the data or calculating the quantile moment.

In addition I have to specify some settings for the plot. Their meaning is mostly straightforward:

``` r
center.offset=1/9 ## If I set the center of the radar plot to zero I get problems with overplotting (all minimum values will be at (0,0)). Therefore it is wise to move the minium line away from the center. I use 1/9 by default. 
grid.max=1  ## how far shall the 100% line be from the center 
axis.label.offset=1.15 ## If bigger than one, then the axis.label will be further out than the maximum line. Default is 1.15
n_grid=4 ## how many gridlines without the 0% line. Grid vector is created using seq(0,1,1/n). Default is 4 to create a 25%, 50%, 75% and 100% description. Please note that the interpretation varies, depending on whether you calculate quantile moments or just rescale between zero and one.  
r=seq(0,1,1/n_grid) ## radius for all grid lines
```

Calculating outer grid line of the radar/spider plot
----------------------------------------------------

The first function I started to work on was based on the idea of a Stackoverflow user. The idea here (as well as in all the following functions) is to draw a circle. A minimal radar plot consists of two circles: An inner circle where all the minimum data points lie and an outer circle where all the outer datapoints lie. The `ggradar`-package uses the `funcCircleCoords`-function to draw each of these circles.

The function draws a polygon with multiple evenly spaced 'corners'. The `center`-argument gives where in the cartesian coordinate sytsem the center of the polygon lies. Then the `r`-argument indicates the radius, meaning what distance the polygon has from `center`. Now if we set the `npoints` argument to a very high number (for me anything over 100 does it), it looks as if we have circle. Even though in reality we have a polygon with more than 100 evenly spaced 'corners'.

First the function creates a sequence of `npoints` evenly spaced between zero and `2*pi`(~6,28). Then the *y-axis* of all polygon points is calculated by multiplying the radius with the *cosinus* of the sequence we created before. The same is done for the *x-axis*, but here we wrap the sequence into a *sinus*-function. Then we give out the result as a dataframe.

Since this function is only doing the operation for a single radius value, I tested whether you even need the function. Therefore I compared an implementation of the function (`myfun1`) with the way I would calculate the circle (`myfun2`).

``` r
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

``` r
microbenchmark::microbenchmark(myfun1(),myfun2(),times=1000L)
```

    ## Unit: microseconds
    ##      expr     min      lq      mean  median       uq     max neval
    ##  myfun1() 219.202 238.101 335.90490 269.351 328.9015 19894.5  1000
    ##  myfun2()  24.501  32.101  61.13581  40.301  50.6005 13484.4  1000

My approach is a lot faster. This might be because the `ggradar`-function uses dataframes in the output, whereas I use matrices. In addition the `ggradar`-version is not fully adapted to my problem. With adapt I mean that for the application on a radar plot it doesn't matter where the center is. Therefore I removed the center option. For convenience and easier understanding I am setting the center at (0,0) in the cartesic coordinate system.

With these updates of `funcCircleCoords` the approaches should have the same speed:

``` r
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

    ## Unit: microseconds
    ##      expr    min     lq     mean  median     uq       max neval
    ##  myfun1() 20.602 22.501 53.16239 23.7000 37.401 19580.302  1000
    ##  myfun2() 24.301 26.501 41.99702 27.7005 44.151  2265.401  1000

``` r
microbenchmark::microbenchmark(myfun2(),myfun1(),times=1000L)
```

    ## Unit: microseconds
    ##      expr    min     lq     mean median      uq     max neval
    ##  myfun2() 23.901 25.201 29.08311 25.701 26.6010 206.801  1000
    ##  myfun1() 19.901 21.501 24.40889 22.001 22.8005 260.401  1000

Now the function is slightly faster than handwritten code. Therefore I would only use the function if I need to calculate this stuff more than once.

Transform data points to compare into 'circular' space
------------------------------------------------------

The problem of representing different datapoints on radar plot is quite similar to the one discussed before. If you draw a circle you have one radius for each circle you draw, now if you want to represent different datapoints on a radar plot you first need to know where on the circumference you need your datapoints.

A radar/spider plot usually has straight lines from the center towards the end of the radar. In the `ggradar`-package they were called 'axis'. You have one of these 'axis' for each category you want to use for comparison, i.e., the categories `hp`,`mpg` and 'cyl' in the `mtcars` data. All the different 'axis' have the same distance to each other.

Since all 'axis' need to have the same distance to each other you now that you will need as many 'axis' as categories. So where on the circumference are now the datapoints? Good old *π* will help you. The circumference of a circle is 2*π*, hence if we have a sequence of *n* steps starting at 0 and ending at 2*π*, we go full circle (because sin(0)=sin(2*π*) and cos(0)=cos(2*π*)). Therefore to reduce overplotting we need to stop shortly before 2*π*

Thus our sequence needs to stop at some *t* &lt; 2*π*. The optimal *t* is such that all column names are plotted with even distance to each other. It turns out that we can calculate *t* using a constant *k* such that *t* = *k* \* 2 \* *π* &lt; 2 \* *π* (meaning ⇒*k* &lt; 1). Doing some trial and error I ended up with $k=\\frac{n-1}{n}$, where *n* is total count of all columns used for comparison.

Does that guessed solution work? If *k* = 1 then sin(2 \* *π* \* 1)=sin(0) and we get overplotting. We need to divide by *n*, since that secures even spacing of the ploted column names and we need *n* − 1 in the nominator, because our first column name gets plotted at 0.

With this spacing our datapoints will always stay on the same 'axis' in the circumference. But how do we set their 'height'? It turns out that we don't need to take care of that explicitly: To speak in the language of the `funcCircleCoords` each datapoint has a different radius value (because we rescale each datapoint between zero and one).

This similarity between `funcCircleCoords` and `CalculateGroupPath` is what distinguishes my package so much from `ggradar`. My rewritten `CalculateGroupPath`-function looks like this:

``` r
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

Here i managed to speed up the old `ggradar`-function by wrapping some core functionalities into a `lapply`-loop. The function takes all column names to compare, the minimum grid line (where the minima data points are) and the maximum grid line (maxima data points).

The question now is which of the three functions in the fastest in calculating these two 'circles'? Which will be the fastest function - `CalculateAxisPath` - `funcCircleCoords` or the last version of - `CalculateGroupPath`?

``` r
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

    ## Unit: microseconds
    ##      expr    min     lq      mean median       uq     max neval
    ##  myfun1() 55.001 61.701 104.67821 66.901 112.0005 21467.6 10000
    ##  myfun2() 44.101 49.001  87.33522 53.901  92.2000 34993.7 10000
    ##  myfun3() 44.501 50.001  84.16676 54.800  91.7010 17279.3 10000

The slowest function is `CalculateAxisPath`, while `CalculateGroupPath6` and `funcCircleCoords` are on par. However both do the precisely same thing and since it doesn't really make sense to use three very similar functions for the same stuff, I will use `CalculateGroupPath6` as the only helper function in my new update. Mainly because it is the most flexible function of the three and doesn't need any ugly `lapply`-loop for speed like `funcCircleCoords`.

### Some common sense

For a radar plot there are four components to be calculated

-   the grid lines
-   the 'axis' paths/lines
-   the positions of the labels
-   positions of the datapoints to compare

Now the grid lines and the 'axis' paths share points. Basically the 'axis' paths are a subset of the grid lines (they can be equal to the grid lines if there is only a minimum and maximum grid line). Therefore I wanted to check which is faster: Calculating the 'axis' paths or extracting them from the grid lines.

``` r
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

    ## Unit: microseconds
    ##      expr  min      lq     mean  median      uq       max neval
    ##  myfun1()  9.3 11.6010 43.33926 20.6000  24.901 21701.602  1000
    ##  myfun2() 44.3 48.9005 98.80819 86.7505 102.901  4324.801  1000

``` r
microbenchmark::microbenchmark(myfun2(),myfun1(),times=1000L)
```

    ## Unit: microseconds
    ##      expr    min     lq     mean median     uq     max neval
    ##  myfun2() 44.500 47.402 63.93289 49.101 63.501 640.001  1000
    ##  myfun1()  9.301 10.701 14.70930 11.601 13.851 308.401  1000

Appendix
========

Benchmark of all `CalculateGroupPath`-functions
-----------------------------------------------

``` r
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

    ## Unit: microseconds
    ##                        expr       min         lq       mean     median
    ##   CalculateGroupPath(mymat) 30821.002 34710.4500 41408.7643 37105.5505
    ##  CalculateGroupPath2(mymat)   400.901   452.7505   693.5866   500.5510
    ##  CalculateGroupPath3(mymat)   367.701   411.6015   584.6632   457.7010
    ##  CalculateGroupPath4(mymat)   318.300   358.6510   511.0074   400.8010
    ##  CalculateGroupPath5(mymat)   302.801   342.8005   512.3818   378.7005
    ##  CalculateGroupPath6(mymat)    40.000    66.3005   113.8222    77.7010
    ##          uq      max neval
    ##  42525.3010 205823.2  1000
    ##    676.6015  29986.0  1000
    ##    582.7510  19523.4  1000
    ##    523.3515  14482.0  1000
    ##    506.2510  29552.9  1000
    ##    100.9510  16956.0  1000

Calculating the 'axis' lines
----------------------------

``` r
CalculateAxisPath <- function(var.names,min,max) {
  #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
  #Args:
  #var.names - list of variables to be plotted on radar plot
  #min - MININUM value required for the plotted axes (same value will be applied to all axes)
  #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
  #var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  #Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i,min.x[i],min.y[i])
    b <- c(i,max.x[i],max.y[i])
    axisData <- rbind(axisData,a,b)
  }
  #Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no","x","y")
  rownames(axisData) <- seq(1:nrow(axisData))
  #Return calculated axis paths
  as.data.frame(axisData)
}

CalculateAxisPath2 <- function(var.names,min,max) {
  n<-length(var.names)
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  tmp<-lapply(1:n,function(i) matrix(c(i,i,min.x[i],max.x[i],min.y[i],max.y[i]),2,3))
  res<-as.data.frame(do.call(rbind,tmp))
  colnames(res) <- c("axis.no","x","y")
  return(res)
}


CalculateAxisPath3 <- function(var.names,min,max) {
  n<-length(var.names)
  #Cacluate required number of angles (in radians)
  angles <- seq(from=0, to=2*pi, by=(2*pi)/n)
  res<-data.frame(axis.no=rep(1:n,each=2), x=rep(c(min,max),n)*sin(rep(angles[-(n+1)],2)),
                  y=rep(c(min,max),n)*cos(rep(angles[-(n+1)],2)))
  return(res)
}

CalculateAxisPath4 <- function(var.names,min,max) {
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


microbenchmark::microbenchmark(CalculateAxisPath(colnames(mat_radar),0+abs(center.offset),grid.max+abs(center.offset)),
                               CalculateAxisPath2(colnames(mat_radar),0+abs(center.offset),grid.max+abs(center.offset)),
                               CalculateAxisPath3(colnames(mat_radar),0+abs(center.offset),grid.max+abs(center.offset)),
                               CalculateAxisPath4(colnames(mat_radar),0+abs(center.offset),grid.max+abs(center.offset)),
                               times=1000L)
```

    ## Unit: microseconds
    ##                                                                                                 expr
    ##   CalculateAxisPath(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
    ##  CalculateAxisPath2(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
    ##  CalculateAxisPath3(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
    ##  CalculateAxisPath4(colnames(mat_radar), 0 + abs(center.offset),      grid.max + abs(center.offset))
    ##      min       lq     mean  median       uq     max neval
    ##   92.502 110.0510 168.3631 122.151 153.0010 17071.2  1000
    ##   79.800  91.1510 139.7754 102.501 127.5510 12730.3  1000
    ##  213.102 243.7510 357.4995 264.251 344.5005 11537.1  1000
    ##   54.601  64.1505 112.8734  71.900  93.9005 14085.7  1000
