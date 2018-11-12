#Data was in excel, so first thing is importing it to R.
library(readxl)
default <- read_excel("ccdefault.xls",col_names=TRUE, skip=1) #skipped the first row, because wanted to assign column names from the second row
default.clean <- default[,-1] #deleted ID column

save(default.clean,file="default.RData")

el.type <- sample(size=nrow(default.clean),c("train","test"),replace = TRUE,prob=c(0.7,0.3))

default.train <- default.clean[el.type == "train",]
default.test <- default.clean[el.type == "test",]

logisticReg.model1 <- glm(default_next_month ~., family=binomial, data = default.clean)
summary(logisticReg.model1) #If Warning message glm.fit: fitted probabilities numerically 0 or 1 occurr it means that the model is predicting absolute probabilities like 0 and 1. I think it is ok here.

res.train.probs0 <- predict(logisticReg.model1, newdata = default.train, type = "response")

cat("\nProbabilities (training set):\n")
print(head(res.train.probs0))

res.train.class0 <- ifelse(res.train.probs0 > 0.5, 1, 0)
cat("\nClasses (training set):\n")
print(head(res.train.class0))

res.test.probs0 <- predict(logisticReg.model1, newdata = default.test, type = "response")
cat("\nProbabilities (test set):\n")
print(head(res.test.probs0))

res.test.class0 <- ifelse(res.test.probs0 > 0.5, 1, 0)
cat("\nClasses (test set):\n")
print(head(res.test.class0))

tb.train0 <- table(Observed=default.train$default_next_month,Predicted=res.train.class0)

cat("\nContingency table (training set)\n")
print(tb.train0)

accuracy.train0 <- sum(diag(tb.train0))/sum(tb.train0)
cat(paste("\nAccuracy (training set): ",accuracy.train0,"\n"))

tb.test0 <- table(Observed=default.test$default_next_month,Predicted=res.test.class0)

cat("\nContingency table (test set)\n")
print(tb.test0)

accuracy.test0 <- sum(diag(tb.test0))/sum(tb.test0)
cat(paste("\nAccuracy (test set): ",accuracy.test0,"\n"))
# Based on results (significance of independent variables) let's build the second model

logisticReg.model2 <- glm(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2, family=binomial, data = default.clean)
summary(logisticReg.model2)

# The amount of independent variables in model 2 is 10, in model 1 - 23. Model 2 is simplier and AIC isn't significantly different.

lr.model <- glm(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2, family=binomial, data = default.train)

print(summary(lr.model))

res.train.probs <- predict(lr.model, newdata = default.train, type = "response")

cat("\nProbabilities (training set):\n")
print(head(res.train.probs))

res.train.class <- ifelse(res.train.probs > 0.5, 1, 0)
cat("\nClasses (training set):\n")
print(head(res.train.class))

res.test.probs <- predict(lr.model, newdata = default.test, type = "response")
cat("\nProbabilities (test set):\n")
print(head(res.test.probs))

res.test.class <- ifelse(res.test.probs > 0.5, 1, 0)
cat("\nClasses (test set):\n")
print(head(res.test.class))

tb.train <- table(Observed=default.train$default_next_month,Predicted=res.train.class)

cat("\nContingency table (training set)\n")
print(tb.train)

accuracy.train <- sum(diag(tb.train))/sum(tb.train)
cat(paste("\nAccuracy (training set): ",accuracy.train,"\n"))

tb.test <- table(Observed=default.test$default_next_month,Predicted=res.test.class)

cat("\nContingency table (test set)\n")
print(tb.test)

accuracy.test <- sum(diag(tb.test))/sum(tb.test)
cat(paste("\nAccuracy (test set): ",accuracy.test,"\n"))

#Logistic model#2 without any data about previous payments

lr.model2 <- glm(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE, family=binomial, data = default.train)

print(summary(lr.model2))

res.train.probs2 <- predict(lr.model2, newdata = default.train, type = "response")

cat("\nProbabilities (training set):\n")
print(head(res.train.probs2))

res.train.class2 <- ifelse(res.train.probs2 > 0.5, 1, 0)
cat("\nClasses (training set):\n")
print(head(res.train.class2))

res.test.probs2 <- predict(lr.model2, newdata = default.test, type = "response")
cat("\nProbabilities (test set):\n")
print(head(res.test.probs2))

res.test.class2 <- ifelse(res.test.probs2 > 0.5, 1, 0)
cat("\nClasses (test set):\n")
print(head(res.test.class2))

tb.train2 <- table(Observed=default.train$default_next_month,Predicted=res.train.class2)

cat("\nContingency table (training set)\n")
print(tb.train2)

accuracy.train2 <- sum(diag(tb.train2))/sum(tb.train2)
cat(paste("\nAccuracy (training set): ",accuracy.train2,"\n"))

tb.test2 <- table(Observed=default.test$default_next_month,Predicted=res.test.class2)

cat("\nContingency table (test set)\n")
print(tb.test2)

accuracy.test2 <- sum(diag(tb.test2))/sum(tb.test2)
cat(paste("\nAccuracy (test set): ",accuracy.test2,"\n"))
