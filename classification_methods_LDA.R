library(MASS)

lda.model <- lda(default_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2, data = default.train)

res.train <- predict(lda.model,newdata=default.train)
res.test <- predict(lda.model,newdata=default.test)

cat("\nProbabilities (training set):\n")
print(head(res.train$posterior))

cat("\nClasses (training set):\n")
print(head(res.train$class))

tb.train<-table(Observed=default.train$default_next_month,Predicted=res.train$class)

cat("\nContingency table (training set):\n")
print(tb.train)

accuracy.train <- sum(diag(tb.train))/sum(tb.train)
cat(paste("\nAccuracy (training set): ",accuracy.train,"\n"))

cat("\nProbabilities (test set):\n")
print(head(res.test$posterior))

cat("\nClasses (test set):\n")
print(head(res.test$class))

tb.test<-table(Observed=default.test$default_next_month,Predicted=res.test$class)

cat("\nContingency table (test set):\n")
print(tb.test)

accuracy.test <- sum(diag(tb.test))/sum(tb.test)
cat(paste("\nAccuracy (test set): ",accuracy.test,"\n"))