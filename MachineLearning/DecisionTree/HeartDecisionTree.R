filepath = "E:/Git/rprojects/MachineLearning/DecisionTree/heart.csv"

library(rpart)

heart=read.csv(filepath,sep="")

str(heart)

#cast the cols to correct types 
cols = c(1,4,5,8,10,12)
heart[,cols] = as.numeric(as.character(unlist(heart[,cols])))

cols = c(2,3,6,7,9,13)
heart[,cols] = lapply(heart[,cols],factor)
cols = c(11)
heart[,cols] = as.integer(as.character(unlist(heart[,cols])))


#build decision tree
heartTree = rpart(heart.disease ~ ., data=heart,method = 'class')
plot(heartTree)
text(heartTree,pretty = T)

#R code for overfitting
#heartTree = rpart(heart.disease ~ ., data=heart,method = 'class',
#                  control =rpart.control(minsplit = 1, cp=0.0001))


#Plot beautiful graphs
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')

library(rattle)	
library(rpart.plot)
library(RColorBrewer)
	
fancyRpartPlot(heartTree)

