setwd("C:/Users/Prudhvi/Desktop/data science/ML lab/rules") 

rm(list=ls(all=TRUE))
steel= read.csv("annealingbinnedData.csv", header = T)

str(steel)
summary(steel)

steel_tran <- as(steel, "transactions") 
library(arules)
itemFrequency(steel_tran)
itemFrequencyPlot(steel_tran)
itemFrequencyPlot(steel_tran, support = 0.5, cex.names=0.8)

rules <- apriori(steel_tran,parameter = list(support = 0.06, confidence = 0.2))
inspect(rules)
 
rules_total = as(rules,"data.frame")
rules3 <- subset(rules, subset = rhs %in% "class=3" & lift > 1.01)
rules_3s <- as(rules3,"data.frame")
inspect(rules3)

write.csv(rules_3s,"steel3.csv") 

rules2 <- as(subset(rules, subset = rhs %in% "class=2"),"data.frame")


rules5 <- subset(rules, subset = lhs %in% "class=5")
rules_5s = as(rules5,"data.frame")


rules_oth <- subset(rules, subset = lhs %in% "class=others" & lift > 1.01)


rules2.sorted <- rules2[order(rules2$lift,decreasing = T),]
