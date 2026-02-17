cntworker2= function(last,first,sd){
  
  x = seq(0,last,by=0.01)
  y =rep(0, (length(x)))
  z =rep(0, (length(x)))
  y[1] = z[1] = first
  range = (1:length(x))
  for (i in range[-1]) {
    y[i] = y[i-1]+rnorm(1,mean = 0,sd)
    z[i] = z[i-1]+rnorm(1,mean = 0,sd)
    #    print(y)
  }
  
  
  df = data.frame(x,y,z)
  print(df)
  plot(df$x,df$y,xlab = "Time",ylab = "Value",col = "red")
  lines(df$x,df$z,col="blue",,type = "p")
  #print(x)
  #print(y)
}

