# Decision tree model building and testing

library(rpart)
library(rpart.plot)

tree.model1<-rpart(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2,
                   data=default.train)

# Decision tree graph
rpart.plot(tree.model1, main="Decision tree model 1")

# Detailed information about tree model
cat("\n\nTree model information: Decision tree model 1\n\n")
print(tree.model1)


# Model complexity: 

# minsplit – 	the minimal number of observations that must exist in a node 
# to consider the split
# the default value: 20

# cp (complexity parameter) – complexity parameter. The minimal decrease of the homogeneity
# measure which must be achieved performing a split. If a split does not achieve this value, 
# then it is not considered (and performed)
# the default value: 0.01



tree.model2 <- rpart(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2,
                     data=default.train, control=rpart.control(minsplit=5,cp=0.002))

rpart.plot(tree.model2,main="Decision tree model 2", clip.facs = TRUE)
cat("\n\nTree model information: Decision tree model 2\n\n")
print(tree.model2)

#res.train2 <- predict(tree.model2,newdata=default.train)
#
#res.train.class2 <- ifelse(res.train2 > 0.5, 1, 0)
#tb.train<-table(Observed=default.train$default_next_month,Predicted=res.train.class2)
#cat("\nClasses (training set):\n")
#print(head(res.train.class2))

#cat("\nContingency table (training set):\n")
#print(tb.train)


tree.execute <- function(tree.model, dataset) {
  
  res.t <- predict(tree.model,newdata=dataset)
  
  res.t.class <- ifelse(res.t > 0.5, 1, 0)
  
  tb.t<-table(Observed=dataset$default_next_month,Predicted=res.t.class)
  
  print(head(res.t.class))
  
  cat("Contingency table:\n\n")
  print(tb.t)
  
  accuracy <- sum(diag(tb.t))/sum(tb.t)
  cat("Accuracy: ",accuracy,"\n\n")

}

cat("---------------------------------\n")
cat("Model 1 execution (training set):\n")
cat("---------------------------------\n")
tree.execute(tree.model1,default.train)

cat("------------------------------\n")
cat("Model 1 execution (test set):\n")
cat("------------------------------\n")
tree.execute(tree.model1,default.test)

cat("---------------------------------\n")
cat("Model 2 execution (training set):\n")
cat("---------------------------------\n")
tree.execute(tree.model2,default.train)

cat("------------------------------\n")
cat("Model 2 execution (test set):\n")
cat("------------------------------\n")
tree.execute(tree.model2,default.test)
