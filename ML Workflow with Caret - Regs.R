#####################################################
####   Caret workflow - Regression - LM - GBM - RF
library(tidyverse)
library(caret)
install.packages("caTools")
library(caTools)

## get data
diamonds

## set up train and trainControl
## here we do 10-fold CV
model <- train( Price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model ## very important - you can use that object directly to do a regular predict() call!

## special case: if you want to do repeated CV, then you can adapt the TrainControl object like so:
traincon <-  trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 5, 
  verboseIter = TRUE
)

## GLM workflow with custom trainControl object: myControl - allows you to use same trainControl in lots of different models!
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary, # use twoClassSummary for AUC instead of accuracy 
  classProbs = TRUE, # IMPORTANT - returns class probabilities
  verboseIter = TRUE
)
## then....
# Train glm with custom trainControl: model
model <- train(
  Class ~ ., 
  train,
  method = "glm",
  trControl = myControl
)
model   # Print model to console


## plot ROC curve:
caTools::colAUC(predictions, test$Class, plotROC = TRUE)

