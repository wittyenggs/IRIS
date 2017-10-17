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
  
  cm <- model_logistic(train,test)
  print('1')
}


model_logistic <- function (train,test){
  
  
  #Logistic Regression
  model <- multinom(Species ~., train)
  Predicted_Species <- predict(model,test, type = 'class')
  cm <- confusionMatrix(Predicted_Species,test$Species)
  return(cm)
}

main()