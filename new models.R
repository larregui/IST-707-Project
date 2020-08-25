
#fit1 <- rpart(heart~target + other variables, data=heart_train, method="class")
plot(fit1)
text(fit1)
rpart.plot(fit1, roundint = FALSE , digits = 4)

#fit2 <- rpart(heart~target + other variables, data=heart_train, method="class")
plot(fit2)
text(fit2)
rpart.plot(fit2, roundint = FALSE , digits = 4)

#fit3 <- rpart(heart~target + other variables, data=heart_train, method="class")
plot(fit3)
text(fit3)
rpart.plot(fit3, roundint = FALSE , digits = 4)

# NaiveBayes

naiveBayes.model <- train(target ~ .,
  data = heart_train,
  method = "naive_bayes", trControl = trainControl(
    method = "cv",
    number = 10, returnResamp = "all",
    classProbs = TRUE, summaryFunction = twoClassSummary
  ), metric = "ROC"
)
naiveBayes.model # Accuracy - 

feature_importance1 <- varImp(naiveBayes.model, scale = FALSE)
plot(feature_importance1)

yhat.naiveBayes <- predict(naiveBayes.model, heart_test, type = "prob")

naiveBayes_data <- prob.prediction(yhat.model = yhat.naiveBayes)

cfm1 <- confusionMatrix(yhat.naiveBayes, heart_test$target)
cfm1

# rForest

rforest.model <- train(target ~ .,
  data = heart_train,
  method = "rf", trControl = trainControl(
    method = "cv", number = 10,
    returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary,
    seeds = vector(mode = "list", length = nrow(heart_train) + 1) %>%
      lapply(., function(x) 1:20)
  ), metric = "ROC", ntree = 20, importance = TRUE
)
rforest.model # Accuracy - 

feature_importance2 <- varImp(rforest.model, scale = FALSE)
plot(feature_importance2)

yhat.rforest <- predict(rforest.model, heat_test, type = "prob")

cfm2 <- confusionMatrix(yhat.rforest, heart_test$target)
cfm2

rforest_data <- prob.prediction(yhat.model = yhat.rforest)

#kNN

knn.model <-
  train(target ~ .,
    data = heart_train, method = "knn",
    trControl = trainControl(
      method = "cv", number = 10,
      returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary
    ),
    metric = "ROC"
  )
knn.model # Accuracy - 

feature_importance3 <- varImp(knn.model, scale = FALSE)
plot(feature_importance3)

yhat.knn <- predict(knn.model, newdata = heart_test, type = "prob")
                       
cfm3 <- confusionMatrix(yhat.knn, heart_test$target)
cfm3

knn_data <- prob.prediction(yhat.model = yhat.knn)

# SVM model

svm.model <-
  train(target ~ .,
    data = heart_train, method = "svmLinear2",
    trControl = trainControl(
      method = "cv", number = 10,
      returnResamp = "all", classProbs = TRUE
    ),
    tuneGrid = data.frame(cost = c(.25, .5, 1))
  )
svm.model # Accuracy - 

feature_importance4 <- varImp(svm.model, scale = FALSE)
plot(feature_importance4)

yhat.svm <- predict(svm.model, heart_test, type = "prob")

cfm4 <- confusionMatrix(yhat.svm, heart_test$target)
cfm4
                       
svm_data <- prob.prediction(yhat.model = yhat.svm)


