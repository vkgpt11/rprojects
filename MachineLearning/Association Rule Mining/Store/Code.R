getwd()

sales = read.csv("Global Superstore.csv")

str(sales)
trans <- sales[,c(2,17)]

#Data preparation for reading transactions
trans1<- unique(trans)
trans1$Order.ID<- as.character(trans1$Order.ID)
trans1$Sub.Category<- as.character(trans1$Sub.Category)

new_data<- paste("\n",trans1$Order.ID, " ", trans1$Sub.Category)

cat(new_data)

write(new_data, file = "demo_single")



#reading the data as transaction format
tr <- read.transactions("demo_single", format = "single", cols = c(1,2))

#alternatively, we could have used as() to convert the data into transaction format after 
#appropriate data preparation


###########################  Checkpoint 2: (Association Rule Mining)  ##############################

#View the summary of transactions                                             
summary(tr) 

#see the transactions
inspect(tr)

#plot the item freqeuncy
itemFrequencyPlot(tr,type="absolute")

#plot the item freqeuncy
itemFrequencyPlot(tr,type="relative")

#plot the item frequency with minimum support of 10%
itemFrequencyPlot(tr,type="relative", support=0.1)

#figuring out the appropriate value of support,  confidence & minlen through logical trial & error.
rules2 <- apriori(tr, parameter = list(support=0.02, confidence=0.01, minlen=2))


top5<-head(rules2, n=5, by= "confidence")

inspect(top5)



rules3 <- apriori(tr, parameter = list(support=0.005, confidence=0.01, minlen=3))

top5<-head(rules3, n=5, by= "confidence")

inspect(top5)

rules_without_binders2 <- subset(rules2,(subset= rhs %in% c('Accessories',
                                                            'Appliances',
                                                            'Bookcases',
                                                            'Chairs',
                                                            'Copiers',
                                                            'Envelopes',
                                                            'Fasteners',
                                                            'Furnishings',
                                                            'Labels',
                                                            'Machines',
                                                            'Paper',
                                                            'Phones',
                                                            'Supplies',
                                                            'Tables',
                                                            'Storage',
                                                            'Art')))

top5<-head(rules_without_binders2, n=5, by= "confidence")

inspect(top5)

rules_without_binders3 <- subset(rules3,(subset= rhs %in% c('Accessories',
                                                            'Appliances',
                                                            'Bookcases',
                                                            'Chairs',
                                                            'Copiers',
                                                            'Envelopes',
                                                            'Fasteners',
                                                            'Furnishings',
                                                            'Labels',
                                                            'Machines',
                                                            'Paper',
                                                            'Phones',
                                                            'Supplies',
                                                            'Tables',
                                                            'Storage',
                                                            'Art')))

top5<-head(rules_without_binders3, n=5, by= "confidence")

inspect(top5)

#Since, binders was occuring too often, it didn't make any sense to include it into the rules. 
#A customer would anyway have bought the item. So we excluded the binders from the RHS and came up
# with new set of rules.
#Similarly students can come up with their own business rules & interpretations

