
#http://www.stat.berkeley.edu/~spector/s133/Clus.html

cars = read.delim("http://www.stat.berkeley.edu/classes/s133/data/cars.tab",stringsAsFactors = FALSE)

head(cars)


#It looks like the variables are measured on different scales, so we will likely want to standardize the data before proceeding
#The daisy function in the cluster library will automatically perform standardization,but it doesn't give you complete control
#we want to standardize by subtracting the median and dividing by the mean average deviation
#You pass scale a matrix or data frame to be standardized, and two optional vectors.
cars.use = cars[,-c(1,2)]
head(cars.use)
medians = apply(cars.use,2,median)
mads = apply(cars.use,2,mad)
cars.use = scale(cars.use,center=medians,scale=mads)
#(The 2 used as the second argument to apply means to apply the function to the columns of the matrix or data frame; a value of 1 means to use the rows.)
cars.dist = dist(cars.use)
head(cars.dist)


cars.hclust = hclust(cars.dist)


plot(cars.hclust,labels=cars$Car,main='Default from hclust')

groups.3 = cutree(cars.hclust,3)

table(groups.3)

#To see the sizes of the clusters for solutions ranging from 2 to 6 clusters
counts = sapply(2:6,function(ncl)table(cutree(cars.hclust,ncl)))
names(counts) = 2:6
count

cars$Car[groups.3 == 1]
sapply(unique(groups.3),function(g)cars$Car[groups.3 == g])

groups.4 = cutree(cars.hclust,4)
sapply(unique(groups.4),function(g)cars$Car[groups.4 == g])

table(groups.3,cars$Country)
aggregate(cars.use,list(groups.3),median)
a3 = aggregate(cars[,-c(1,2)],list(groups.3),median)
data.frame(Cluster=a3[,1],Freq=as.vector(table(groups.3)),a3[,-1])


a4 = aggregate(cars[,-c(1,2)],list(groups.4),median)
data.frame(Cluster=a4[,1],Freq=as.vector(table(groups.4)),a4[,-1])


library(cluster)
cars.pam = pam(cars.dist,3)
names(cars.pam)
table(groups.3,cars.pam$clustering)

#The solutions seem to agree, except for 1 observations that hclust put in group 2 and pam put in group 3. 
#Which observations was it?
cars$Car[groups.3 != cars.pam$clustering]

plot(cars.pam)
