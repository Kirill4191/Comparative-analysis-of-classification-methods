library(RSNNS)
library(rgl)
library(devtools)
library(reshape)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(NeuralNetTools)

# Data splitting
# Data were splitted before into default.train and default.test (as 70%:30%)

inputsTrain <- default.train[,-c(8:11,13:17,20:24)]
inputsTest <- default.test[,-c(8:11,13:17,20:24)]

# Data normalization
inputsTrain <- normalizeData(inputsTrain,type="norm")
paramOfNormInputs <- getNormParameters(inputsTrain)
inputsTest <- normalizeData(inputsTest,type=paramOfNormInputs) #parameters of train set is used here, later for prediction we also will use this parameters, hence we need to save it


# Model building
mlp.model <- mlp(inputsTrain, default.train$default_next_month, size=5, maxit=5000, inputsTest=inputsTest, targetsTest=default.test$default_next_month)


# Error plotting
plotIterativeError(mlp.model)
plotIterativeError(mlp.model,log="y")

# Regression error for training set
plotRegressionError(default.train$default_next_month,mlp.model$fitted.values)

# Regression error for test set
plotRegressionError(default.test$default_next_month,mlp.model$fittedTestValues)

# MSE errors (for normalized data!!!)
mse.train <- sum((default.train$default_next_month-mlp.model$fitted.values)^2)/length(inputsTrain)
mse.test <- sum((default.test$default_next_month-mlp.model$fittedTestValues)^2)/length(inputsTest)
cat("MSE (training test): ",mse.train,"\n")
cat("MSE (test set): ",mse.test,"\n")

#Accuracy check in order to compare NN model to LR and LDA
res.train.class <- ifelse(mlp.model$fitted.values > 0.5, 1, 0)
cat("\nClasses (training set):\n")
print(head(res.train.class))

res.test.class <- ifelse(mlp.model$fittedTestValues > 0.5, 1, 0)
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
###

# Data denormalization and plotting (training set)
#input.train.denorm <- denormalizeData(inputsTrain,paramOfNormInputs)
#output.train.denorm <- denormalizeData(mlp.model$fitted.values,paramOfNormTargets)

# Data denormalization and plotting (test set) #to interpret results
#input.test.denorm <- denormalizeData(inputsTest,paramOfNormInputs)
#output.test.denorm <- denormalizeData(mlp.model$fittedTestValues,paramOfNormTargets)

# Plotting NN model
plot.nnet(mlp.model)

# NN model details
print(extractNetInfo(mlp.model))

# NN model weights
print(weightMatrix(mlp.model))