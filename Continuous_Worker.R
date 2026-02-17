cntworker= function(last,first,sd){
  
  x = seq(0,last,by=0.01)
  y =rep(0, (length(x)))
  y[1] = first
  range = (1:length(x))
  for (i in range[-1]) {
    y[i] = y[i-1]+rnorm(1,mean = 0,sd)
#    print(y)
  }
  
  
  df = data.frame(x,y)
  print(df)
  plot(df$x,df$y,xlab = "Time",ylab = "Value")
  #print(x)
  #print(y)
}