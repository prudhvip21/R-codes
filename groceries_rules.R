rm(list=ls(all=TRUE))
data("Groceries")
groc = as(Groceries,"data.frame")

# making rules out of grocries

rules <- apriori(Groceries, parameter = list(sup = 0.005, conf = 0.05))

# observing the summary of rules
summary(rules)
# converting into data frame
rules1 = as(rules, "data.frame")
itemFrequencyPlot(Groceries)
Groceries

# deriving when milk is brought 
rules.milk <- subset(rules, subset = rhs %in% "whole milk")
rmilk = as(rules.milk,"data.frame")
rules.milk2= as(head(sort(rules.milk, by = "support"), n=10), "data.frame")

# pruning rules
library(arulesViz)
rules_prune <- subset(rules, subset= lift>2.5)
c = Groceries@itemInfo$labels
rprune <- as(rules_prune,"data.frame")

# plotting rules
plot(rules_prune, measure=c("support","lift"), shading="confidence");
subrules2 = head(sort(rules_prune, by="lift"), 30);
finalrules = as(subrules2, "data.frame")
write.csv(finalrules,"rules_groceries.csv")

# find redundant rules
subset.matrix <- is.subset(rules_prune, rules_prune)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules_prune[!redundant]
rules_pruned_final <- as(rules.pruned, "data.frame")
inspect(rules.pruned)

# finding rules for each item 
for (i in 1:169)
{
  rules.sample = subset(rules,subset = rhs %in% c[i])
  
  rules.sample2= as(head(sort(rules.sample, by = "confidence"), n=1), "data.frame")
  print(rules.sample2)
  rsample2= as(rules.sample2,"data.frame")
  
}

write.csv(rsample,"Groceries_rules.csv")
