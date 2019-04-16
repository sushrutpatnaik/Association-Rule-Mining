
library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)
library(ggplot2)

ordr_pr <- read_csv("https://utdallas.edu/~sxr173830/assignment4/order_products__prior.csv")
#str(order_products_prior)
prods <- read_csv("https://utdallas.edu/~sxr173830/assignment4/products.csv")

deps <- read_csv("https://utdallas.edu/~sxr173830/assignment4/departments.csv")

order_dept <-ordr_pr %>%
  group_by(product_id) %>%
  left_join(prods,by="product_id")

#Join on Product Id to get the product names
order_baskets <- ordr_pr %>% 
  inner_join(prods, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

#Join to get the department names
dept_baskets <- order_dept %>% 
  inner_join(deps, by="department_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(department)))

#Create a transaction class
transactions <- as(order_baskets$basket, "transactions")
#Create a dept transaction class
dep_transactions <- as(dept_baskets$basket, "transactions")

#Finding Frequent Item sets for Productnames in Order dataset

support <- 0.008
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=2), control = list(verbose = FALSE))
par(mar=c(5,18,2,2)+.1)

sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = F))
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:",support), padj = .8) #Paste the output in  the document

#Finding Frequent Item sets for dept

dep_itemsets <- apriori(dep_transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=2), control = list(verbose = FALSE))
par(mar=c(5,18,2,2)+.1)

sets_deps_supp <- DATAFRAME(sort(dep_itemsets, by="support", decreasing = F))
#ggplot(x=sets_deps_supp$support, y=sets_deps_supp$items)
barplot(sets_deps_supp$support, names.arg=sets_deps_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
#mtext(paste("support:",support), padj = .8) #Paste the output in  the document

#Finding Association rules for Productnames in Order Dataset

rules1 <- apriori(transactions, parameter = list(supp = 0.005, conf = 0.1, maxlen=3), control = list(verbose = FALSE))
summary(quality(rules1))
inspect(sort(rules1, by="lift")[1:10]) #take a screenshot of it
inspect(sort(rules1, by="confidence")[1:10]) #take a screenshot of it

#Finding Association rules for dept

rules2 <- apriori(dep_transactions, parameter = list(supp = 0.005, conf = 0.1, maxlen=3), control = list(verbose = FALSE))
summary(quality(rules2))
inspect(sort(rules2, by="lift")[1:10]) #take a screenshot of it
inspect(sort(rules2, by="confidence")[1:10]) #take a screenshot of it
