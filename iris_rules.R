rm(list=ls(all=TRUE)) 
data("iris")
iris

# data visualisation
str(iris)
summary(iris)
boxplot(iris)

# copying iris data
iris_data = iris

# function to bin sepal length 
len.cat <- function(x)
{
  if (x>=4.3 & x< 5.1)
    sep.len = '1'
  else if (x>=5.1 & x< 5.8 )
    sep.len = '2'
  else if (x>=5.8 & x< 6.4)
    sep.len = '3'
  else 
    sep.len = '4'
}

# apply function to length 

sep_cat_len = sapply(iris_data$Sepal.Length,len.cat)

# binning width variable 

wid.cat <- function(x)
{
  if (x>=2 & x< 2.8)
    sep.wid = '1'
  else if (x>=2.8 & x< 3)
    sep.wid = '2'
  else if (x>=3 & x< 3.3)
    sep.wid = '3'
  else 
    sep.wid = '4'
}

sep_cat_wid = sapply(iris_data$Sepal.Width, wid.cat) 

# binning petal length 

plen.cat <- function(x)
{
  if (x>=1 & x< 1.6)
    pet.len = '1'
  else if (x>=1.6 & x< 4.35)
    pet.len = '2'
  else if (x>=4.35 & x< 5.1)
    pet.len= '3'
  else 
    pet.len = '4'
}

pet_cat_len = sapply(iris_data$Petal.Length, plen.cat) 

# binning petal width 

pwid.cat <- function(x)
{
  if (x>=0.1 & x< 0.3)
    pet.wid = '1'
  else if (x>=0.3 & x< 1.3)
    pet.wid = '2'
  else if (x>=1.3 & x< 1.8)
    pet.wid = '3'
  else 
    pet.wid = '4'
}

pet_cat_wid = sapply(iris_data$Petal.Length, pwid.cat) 

# binned vectors into data frame 
iris_final <- data.frame(pet_cat_len,pet_cat_wid,sep_cat_len,sep_cat_wid,iris_data$Species)

# converting into transactions format for rules
iris_rules = as(iris_final, "transactions")

# observing data points 

itemFrequency(iris_rules)
itemFrequencyPlot(iris_rules)

# applying rules 
rules <- apriori(iris_rules,parameter = list(support = 0.06, confidence = 0.6))

# classifyin based on outputs as species 
rules_classifier <- subset(rules, subset = rhs %in% "iris_data.Species=setosa"| rhs %in% "iris_data.Species=versicolor"| rhs %in% "iris_data.Species=virginica" & confidence >0.7)

library(arules)
# find redundant rules
subset.matrix <- is.subset(rules_classifier, rules_classifier)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules_classifier[!redundant]
rules_pruned_final <- as(rules.pruned, "data.frame")
inspect(rules.pruned)
write.csv(rules_pruned_final,"Iris_rules.csv")
