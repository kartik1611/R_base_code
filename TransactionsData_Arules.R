setwd("D:/Academic/CPEE/CPEE18/20160911_Batch18_CSE 7405c_AssociationRules_RCode")
library("arules")
trans = read.transactions(file="Transactions.csv", rm.duplicates=TRUE, format="single",sep=",",cols =c(1,2))
inspect(trans)
itemFrequency(trans)
itemFrequencyPlot(trans)
rules <- apriori(trans,parameter = list(minlen=3,sup = 0.00001,target="rules"))
rules.sorted<-sort(rules,by="lift")
summary(rules)
inspect(rules)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


write(rules.pruned, file = "Items_rulesdata_top20.csv", quote=TRUE, sep = ",", row.names=F)


#Visualization of Association  rules 

library(arulesViz)
library(tcltk)
rulesImp <- rules.pruned[1:3]
inspect(rulesImp)
plot(rulesImp[1:3],method="graph",control=list(type="items"))
plot(rulesImp, method="paracoord", control=list(reorder=TRUE))
