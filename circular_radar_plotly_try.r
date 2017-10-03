### I tried to built the same kind of radar as in gg_circular in the PrepPlot-package. 
### However drawing the circles proved challenging 

### minimal example of drawing multiple circles
tmp = data.frame(type = 'circle',fillcolor = 'rgb(50, 20, 90)',
                 x0 = c(1,3,5), x1 = c(2,4,6),xref = 'x',
                 y0 = c(2,4,6), y1 = c(3,5,7),yref = 'y',
                 stringsAsFactors = FALSE)
myshapes2<-lapply(1:nrow(tmp),function(j) {
  mylist<-as.list(tmp[j,])
  mylist$line = list(color = 'rgb(50, 20, 90)')
  return(mylist)})

plot_ly() %>% 
  add_trace(x=0,y=0,type='scatter',mode='markers') %>%
  layout(shapes=myshapes2)

## my try of a circular grid in plotly
tmp = data.frame(type = 'circle',opacity=0.5,
                 x0 = -(rev(r)+centre.y), x1 = rev(r)+centre.y,xref = 'x',
                 y0 = -(rev(r)+centre.y), y1 = rev(r)+centre.y,yref = 'y',
                 stringsAsFactors = FALSE)
myshapes3<-lapply(1:nrow(tmp),function(j) {
  mylist<-as.list(tmp[j,])
  mylist$fillcolor<-ifelse(j!=nrow(tmp),ifelse(j%%2==0,'rgba(191,190,196,0.5)','rbg(185,185,197,0.5)'),'white')
  mylist$line = list(color = ifelse(j!=nrow(tmp),
                                    ifelse(j%%2==0,'rgb(191,190,196,0.5)','rbg(185,185,197,0.5)'),'white'))
  return(mylist)})

## plotting it
ay <- list(title = "",zeroline = FALSE,showline = FALSE,showticklabels = FALSE, showgrid = FALSE)
ax <- list(title = "",zeroline = FALSE,showline = FALSE,showticklabels = FALSE, showgrid = FALSE)
gridly<-plot_ly() %>% 
  add_trace(x=0,y=0,type='scatter',mode='markers',showlegend=FALSE,hoverinfo='skip') %>%
  layout(xaxis=ax,yaxis=ay,shapes=myshapes3) %>%
  add_trace(data=grid$axis_path,x=~x,y=~y,type='scatter',mode='lines',split=~axis.no,
            line=list(color='rgb(180,180,180)'),opacity=0.4,hoverinfo='skip',showlegend=F)
