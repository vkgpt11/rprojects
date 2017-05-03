library(rpart)
seglength <- 1000 
delta <- 100 
segpoints <- 20 
yref <- 10
df <- data.frame("x"=numeric(),"y"=numeric())
# Generate the synthetic dataset and plot it 
for(i in c(0:2)){
  xvals = seq(i*seglength+delta,(i+1)*seglength-delta,length.out = segpoints)
  ybase = if(i==1) yref else 2*yref
  yvals = ybase + rnorm(segpoints)
  newrows = cbind(xvals,yvals)
  df = rbind(df,newrows)
  
}


names(df) <- c('x','y')
plot(df$x,df$y,ylab = 'Y',xlab = 'X')

#Construct the decision tree and plot the tree
dftree = rpart(y~x,data=df,method = 'anova')
plot(dftree)
text(dftree)
