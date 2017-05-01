###============================================###
###===Clustering on Telecom Churn Case Study===###
###============================================###

setwd("E:/Upgrade-DA/3. Predictive Analytics I/7. Unsupervised Learning - Clustering/6. Ungraded Assignment")
data = read.csv("churn_data_complete.csv")

str(data)
data_required =data.frame(data$change_mou,data$drop_vce_Mean,data$custcare_Mean,data$avgmou)
str(data_required)
summary(data_required)

sum(is.na(data_required))
data_cleaned = na.omit(data_required)

summary(data_cleaned)
str(data_cleaned)

box <- boxplot.stats(data_cleaned$data.change_mou)
out <- box$out
data_noOutliers <- data_cleaned[ !data_cleaned$data.change_mou %in% out, ]
data_cleaned = data_noOutliers



box <- boxplot.stats(data_cleaned$data.drop_vce_Mean)
out <- box$out
data_noOutliers <- data_cleaned[ !data_cleaned$data.drop_vce_Mean %in% out, ]
data_cleaned = data_noOutliers


box <- boxplot.stats(data_cleaned$data.custcare_Mean)
out <- box$out
data_noOutliers <- data_cleaned[ !data_cleaned$data.custcare_Mean %in% out, ]
data_cleaned = data_noOutliers

box <- boxplot.stats(data_cleaned$data.avgmou)
out <- box$out
data_noOutliers <- data_cleaned[ !data_cleaned$data.avgmou %in% out, ]
data_cleaned = data_noOutliers

clus3 <- kmeans(data_cleaned, centers = 3, iter.max = 50, nstart = 50)
clus3
str(clus3)

## Finding the optimal value of K

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(data_cleaned, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)


clus5 <- kmeans(data_cleaned, centers = 5, iter.max = 50, nstart = 50)
clus5
str(clus5)

data_cleaned$Cluster = clus5$cluster
data_cleaned
km_clusters<- group_by(data_cleaned, Cluster)
library(dplyr)

tab1<- summarise(km_clusters, MonthlyPercentageChange=mean(data.change_mou), 
                 CallDrop=mean(data.drop_vce_Mean), 
                 CustomerCareCall=mean(data.custcare_Mean),
                 AverageMonthlyMinutes=mean(data.avgmou))

tab1
require(ggplot2)
ggplot(tab1, aes(x= factor(Cluster), y=MonthlyPercentageChange)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=CallDrop)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=CustomerCareCall)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=AverageMonthlyMinutes)) + geom_bar(stat = "identity")

data_dist<- dist(data_cleaned)
RFM_dist

data_hclust <- hclust(data_dist, method="single")
plot(data_hclust)


## Constructing the dendrogram using complete linkage

data_hclust_complete<- hclust(data_dist, method="complete")
plot(data_hclust_complete)

## Visualising the cut in the dendrogram

rect.hclust(data_hclust_complete, k=5, border="red")

## Making the cut in the dendrogram

clusterCut <- cutree(data_hclust_complete, k=5)

## Appending the ClusterIDs to RFM data





data_cleaned$Cluster = clusterCut
data_cleaned
hclust_clusters<- group_by(data_cleaned, Cluster)

library(dplyr)

tab1<- summarise(hclust_clusters, MonthlyPercentageChange=mean(data.change_mou), 
                 CallDrop=mean(data.drop_vce_Mean), 
                 CustomerCareCall=mean(data.custcare_Mean),
                 AverageMonthlyMinutes=mean(data.avgmou))

tab1
require(ggplot2)
ggplot(tab1, aes(x= factor(Cluster), y=MonthlyPercentageChange)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=CallDrop)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=CustomerCareCall)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(Cluster), y=AverageMonthlyMinutes)) + geom_bar(stat = "identity")

