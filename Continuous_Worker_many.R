cntworkermany = function(num_vectors,initial_value,last,sdinput){


#last = 0.3
#sdinput = 1
#initial_value =98
  num_vectors = num_vectors+1
vector_length = last/0.01+1
vec_range = 1:num_vectors
vec_len_range = 1:vector_length

x = seq(0,last,by=0.01)
z =x[-1]

vectors = vector("list", length = num_vectors)


#set.seed(123)


for (j in vec_range) {
  vectors[[j]] = vector(mode = "numeric", length = vector_length)
  vectors[[j]][1] = initial_value
  for (i in vec_len_range[-1]) {
    
    vectors[[j]][i] = vectors[[j]][i-1]+ rnorm(1, mean = 0, sd=sdinput)
  }
}


for (j in vec_range) {
  #print(paste("Vector", j, ": ",(vectors[[j]])))
  
}
df = data.frame(x,vectors)


#df = data.frame(vectors[[1]],vectors[[2]])
#head(df)

plot(df$x,df[,2],type = "o",col = 1,xlab = "Time", ylab = "Value",ylim = c(45,100))
for (i in 2:num_vectors){
  lines(df$x,df[,i],col=i,type ="o")
}
}
