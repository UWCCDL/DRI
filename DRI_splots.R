DRI_splots = function(sub,x,y,color='black',title='title',xlab='xlab',ylab='ylab') {
  
  #function to create scatterplots
  #sub is the subset of data you'd like to visualize
  #x is the data to be plotted on the x axis, y on the y axis
  #color is optional, defaults to black, but you can provide
  #indices to plot as separate colors
  #title, xlab, and ylab labels the plot/axes
  
  plot = ggplot(sub,aes(x=x, y=y))
  
  (plot+geom_point(aes(color=color))
       +labs(title=title,x=xlab,y=ylab)
       +scale_x_continuous(breaks = round(seq(0,round(max(x))+1,by = 0.1),1))
       +scale_y_continuous(breaks = round(seq(0,round(max(y))+1,by = 0.1),1)))
  
}
