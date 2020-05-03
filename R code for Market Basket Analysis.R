
#Install the libraries and packages
install.packages("arules")
install.packages("arulesViz",dependencies = TRUE)
library(arules)
library(arulesViz)

#Read the grocery files as transactions 
groceries_data<-read.transactions(choose.files(),format = 'basket',sep=',')
summary(groceries_data)


# Item frequency plot for the top 20 sold items
library(RColorBrewer)
itemFrequencyPlot(groceries_data,topN=20,type="absolute",col=brewer.pal(9,'Set3'),main='Items Frequency Plot')

# Building the model with default parameters
association.rules <- apriori(groceries_data)

#Building the model with support and confidence parameters
association.rules <- apriori(groceries_data, parameter = list(supp=0.001, conf=0.8,maxlen=10))

#Summary
summary(association.rules)


#TO remove the redundant rules
subset.rules<-which(colSums(is.subset(association.rules,association.rules))>1)
length(subset.rules)
subset.asso.rules<-association.rules[-subset.rules]
summary(subset.asso.rules)

# inspecting first 15 rules
inspect(subset.asso.rules[1:15])

# Generaring rules only for yogurt
yogurt.asso.rules<- apriori(groceries_data, parameter = list(supp=0.001, conf=0.8,maxlen=10),appearance = list(default = "lhs",rhs = "yogurt"))
inspect(head(yogurt.asso.rules))

# Visualization plot for rules with confidence >0.4 ,sorted with cofidence parameter.
inspect(top10subRules[1:10])
subRules<-subset.asso.rules[quality(subset.asso.rules)$confidence>0.4]
top10subRules <- head(subRules, n=10, by= "confidence")
plot(top10subRules, method = "graph",html = "htmlwidget")

#Parallel coordinates plot sorted by lift parameter
subRules1<-head(subRules,n=10,by="lift")
plot(subRules1, method = "paracoord")





