cntworker3= function(last,first,sd){
  
  x = seq(0,last,by=0.01)
  y = z = w = rep(0, (length(x)))
#  z =rep(0, (length(x)))
  y[1] = z[1] = w[1] = first
  range = (1:length(x))
  for (i in range[-1]) {
    y[i] = y[i-1]+rnorm(1,mean = 0,sd)
    z[i] = z[i-1]+rnorm(1,mean = 0,sd)
    w[i] = w[i-1]+rnorm(1,mean = 0,sd)
    #    print(y)
  }
  
  
  df = data.frame(x,y,z,w)
  print(df)
  plot(df$x,df$y,xlab = "Time",ylab = "Value",type = "o",col = "red")
  lines(df$x,df$z,col="blue",type = "o")
  lines(df$x,df$w,col="green",type = "o")
  #print(x)
  #print(y)
}