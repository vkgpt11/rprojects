filepath="E:\\Git\\rprojects\\MachineLearning\\LogisticsRegression"
setwd(filepath)

#Read file
baseball = read.csv("baseball_allAttributes.csv")

#RS=RunScored
#RA=RunAgainst
#SLG= Slug percentage points
#OBP = On Base percentage points
#BA = Batting Average
summary(baseball)

str(baseball)

sum(is.na(baseball))

#check for outliers 
quantile(baseball$RS,c(.95,.96,.97,.98,.99,1))
quantile(baseball$RA,c(.95,.96,.97,.98,.99,1))
quantile(baseball$OBP,c(.95,.96,.97,.98,.99,1))
quantile(baseball$SLG,c(.95,.96,.97,.98,.99,1))
quantile(baseball$BA,c(.95,.96,.97,.98,.99,1))


#no need for outlier treatment 


#data split to training and testing 

set.seed(1000)
library(caTools)
s=sample.split(0:nrow(baseball),SplitRatio=.7)
table(s)

train_baseball = baseball[s,]
test_baseball = baseball[!s,]

#model creation 
initial_model = glm(Playoffs ~ ., data=train_baseball[,-1],family = "binomial")

summary(initial_model)

#variable selection --> select significant variable using step wise procedure 
#In Stepwise, for every iteration, add and remove each variable at a time
#Check AIC value of the new model obtained after addition and removal of each variable
#based on AIC value, decide which model is better 
#Smaller AIC --> better model


#AIC --> Akaike Information Criteria 
#AIC is similar to adjusted R-square 
#Deviance always decreases and R-square always increases as variables are added
#AIC penalizes addition of variables
#AIC decreases only if deviance decreases by significant amount
#AIC can be used for selecting variables 

best_model = step(initial_model,direction = "both")


#VIF computation
library(car)
vif(best_model) 
# We use the vif cutoff as 3 , 
#selection of the cutoff depends on the business undertanding
#selection -> do not remove variables based on high VIF 
#         -> Look at significance level
#Selection of variable through VIF and Stepwise procedure could lead to statistacally accurate 
#but not the model which makes lots of sense 

#Selection or exclusion of variable should not be done based on AIC or stepwise or p-value or VIf
#There has to be some business possibility and significance of that variable --> Makes sense as subject matter view point


vif(best_model)

#We can see that that RS has more vif compare to OBP, but RS has lots of 
#significant in terms of business We will keep RS and Remove OBP


model2 = glm(Playoffs ~ RS +RA+SLG, data = train_baseball,family = "binomial")



summary(model2)

vif(model2)
# SLG or RS?, SLG in baseball equivalant to strike rate in cricket
# We remove the SLG for the same reasons 


model3 = glm(Playoffs ~ RS + RA, data=train_baseball, family = "binomial")

summary(model3)
vif(model3)
model_final=model3

model3$y

