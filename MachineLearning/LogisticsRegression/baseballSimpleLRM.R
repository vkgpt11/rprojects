setwd("E:\\Git\\rprojects\\MachineLearning\\LogisticsRegression")
baseball = read.csv("baseball.csv")

summary(baseball)

sum(is.na(baseball))

#Training and Testing data
set.seed(2)
s= sample(1:nrow(baseball),0.7*nrow(baseball))
base_train = baseball[s,]
base_test = baseball[-s,]

model_runs = glm(Playoffs ~ RS, family = binomial,data=base_train)
summary(model_runs)

pred_runs = predict(model_runs,base_test,type="response")

base_test$pred = ifelse(pred_runs>.1, "Yes","No")

base_test$pred_prob = pred_runs
table(base_test$Playoffs,base_test$pred)
