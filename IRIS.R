library('caret')
library('nnet')
library('e1071')

main <- function(){
  
  #Load the dataset
  data <- read.csv('IRIS.csv',header = TRUE, sep = ",")
  
  #Normalize the data
  data_norm <- predict(preProcess(data,method= c("range")),data)
  
  #Data Partition
  rows <- sample(nrow(data),floor(nrow(data)*0.70))
  train <- data_norm[rows,]
  test <- data_norm[-rows,]
  
  #Create Models & make predictions on test dataset
  model_logistic <- model(train,'Logistic Regression')
  cm_logistic <- prediction(model_logistic,test)
  
  model_svm <- model(train,'SVM')
  cm_svm <- prediction(model_svm,test)
  
  #Print confusion matrix for all models
  print(as.matrix(cm_logistic))
  print(as.matrix(cm_svm))
  
}


model <- function (train,model_name){
  
  if (model_name == 'Logistic Regression')
    model <- multinom(Species ~., train)
  
  else if (model_name == 'SVM')
    model <- svm(Species ~ ., train)
    
  return(model)
  
}


prediction <- function(model,test){
  
  predictedValues <- predict(model,test, type = 'class')
  cm <- confusionMatrix(predictedValues,test$Species)
  return(cm)
}

main()