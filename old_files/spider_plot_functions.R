CalculateGroupPath <- function(df) {
  #Converts variable values into a set of radial x-y coordinates
  #Code adapted from a solution posted by Tony M to
  #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
  #Args:
  #  df: Col 1 -  group ('unique' cluster / group ID of entity)
  #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
  
  path <- df[,1]
  
  ##find increment
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
  ##create graph data frame
  graphData= data.frame(seg="", x=0,y=0)
  graphData=graphData[-1,]
  
  for(i in unique(as.character(path))){
    pathData = subset(df, df[,1]==i)
    for(j in c(2:ncol(df))){
      #pathData[,j]= pathData[,j]
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,j]*sin(angles[j-1]),
                                            y=pathData[,j]*cos(angles[j-1])))
    }
    ##complete the path by repeating first pair of coords in the path
    graphData=rbind(graphData, data.frame(group=i, 
                                          x=pathData[,2]*sin(angles[1]),
                                          y=pathData[,2]*cos(angles[1])))
  }
  #Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData #data frame returned by function
}

CalculateGroupPath2 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-sapply(1:nrow(df), function (j) c(df[j,-1]*sin(angles[-ncol(df)]),df[j,2]*sin(angles[1])))
  yy<-sapply(1:nrow(df), function (j) c(df[j,-1]*cos(angles[-ncol(df)]),df[j,2]*cos(angles[1])))
  graphData<-data.frame(group=rep(df[,1],each=ncol(df)),x=unlist(xx),y=unlist(yy))
  return(graphData)
}
CalculateGroupPath3 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-lapply(1:nrow(df), function (j) c(df[j,-1]*sin(angles[-ncol(df)]),df[j,2]*sin(angles[1])))
  yy<-lapply(1:nrow(df), function (j) c(df[j,-1]*cos(angles[-ncol(df)]),df[j,2]*cos(angles[1])))
  graphData<-data.frame(group=rep(df[,1],each=ncol(df)),x=unlist(xx),y=unlist(yy))
  return(graphData)
}
CalculateGroupPath4 <- function(df) {
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-c(rbind(t(plot.data.offset[,-1])*sin(angles[-ncol(df)]),t(plot.data.offset[,2])*sin(angles[1])))
  yy<-c(rbind(t(plot.data.offset[,-1])*cos(angles[-ncol(df)]),t(plot.data.offset[,2])*cos(angles[1])))
  graphData<-data.frame(group=rep(df[,1],each=ncol(df)),x=(xx),y=(yy))
  return(graphData)
}
CalculateGroupPath5 <- function(mydf) {
  df<-cbind(mydf[,-1],mydf[,2])
  myvec<-c(t(df))
  angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1)) # find increment
  xx<-myvec*sin(rep(c(angles[-ncol(df)],angles[1]),nrow(df)))
  yy<-myvec*cos(rep(c(angles[-ncol(df)],angles[1]),nrow(df)))
  graphData<-data.frame(group=rep(mydf[,1],each=ncol(mydf)),x=(xx),y=(yy))
  return(graphData)
}

# microbenchmark::microbenchmark(CalculateGroupPath(plot.data.offset),CalculateGroupPath2(plot.data.offset),
#                                +                                CalculateGroupPath3(plot.data.offset),CalculateGroupPath4(plot.data.offset),
#                                +                                CalculateGroupPath5(plot.data.offset),times=1000L)
# Unit: microseconds
# expr       min         lq       mean     median         uq      max neval
# CalculateGroupPath(plot.data.offset) 20768.163 21636.8715 23125.1762 22394.1955 23946.5875 86926.97  1000
# CalculateGroupPath2(plot.data.offset)  6710.485  7137.7615  7835.6810  7363.3995  7782.8815 74225.99  1000
# CalculateGroupPath3(plot.data.offset)  6677.664  7084.2240  7919.7489  7340.0155  7785.7540 93081.97  1000
# CalculateGroupPath4(plot.data.offset)   550.148   614.7620   707.2645   650.2490   687.5815 15756.53  1000
# CalculateGroupPath5(plot.data.offset)   577.634   650.0435   738.7701   684.0945   726.9660 11228.58  1000

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

# microbenchmark::microbenchmark(CalculateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y)),
#                                +                                CalculateAxisPath2(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y)),
#                                +                                CalculateAxisPath3(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y)),times=1000L)
# Unit: microseconds
# expr     min      lq     mean  median       uq      max neval
# CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max +      abs(centre.y))  80.819  95.179 113.0619 102.973 112.8190 2678.122  1000
# CalculateAxisPath2(var.names, grid.min + abs(centre.y), grid.max +      abs(centre.y))  75.486  87.384 102.8749  95.589 104.4095 2483.252  1000
# CalculateAxisPath3(var.names, grid.min + abs(centre.y), grid.max +      abs(centre.y)) 222.356 245.536 280.6102 259.690 280.4070 8886.048  1000


funcCircleCoords <- function(center = centre.y, r = 1, npoints = ncol(plot.data)){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center + r * cos(tt)
  yy <- center + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

myfun2<-function(){
  axis$label <-funcCircleCoords(0,(grid.max+abs(centre.y))*axis.label.offset,ncol(plot.data))[-ncol(plot.data),]
  axis$label$text=axis.labels
}

myfun1<-function(){
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label <- data.frame(text=axis.labels,x=NA,y=NA )   #Labels
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
}

# microbenchmark::microbenchmark(myfun1(),myfun2(),times=1000L)
# Unit: microseconds
# expr     min      lq     mean   median       uq      max neval
# myfun1() 362.633 397.912 676.5177 434.4215 608.1495 6185.278  1000
# myfun2() 232.184 262.951 466.9649 292.2810 393.8105 4212.126  1000
