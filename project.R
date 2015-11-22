setwd("~/Desktop/John Hopkin Data Sci/machine learning/project")

library(caret)

#obtain source files
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="train.csv",method="curl")

#Splitting data into training and testing for cross validation purpose
dataset <- read.csv("train.csv")
inTrain <- createDataPartition(dataset$classe,p=0.7,list=FALSE)
training <- dataset[inTrain,]
testing <- dataset[-inTrain,]

#Exploring Training Data
str(training)
dim(training)
#There are 160 variables.  After careful examination of the variable, some of the variables are statistical properties such 
#kurtosis, min, max, etc.  I will remove thses variables since they are not predictive variables

#Data Preprocessing

#identify columns with statistical properties
stats_id <- c("kurtosis","skewness","max","min","amplitude","avg","stddev","var")
col_remove <-c()
for(i in stats_id){
  k <- grep(i, names(training), value = F)
  col_remove <- c(col_remove, as.integer(k))
}

#The first 7 columns are housekeeping items and also need to be removed
col_remove <- c(c(1:7),col_remove)

#Remove unwanted columns
training <- training[,-col_remove]
testing <- testing[,-col_remove]
dim(training)
dim(testing)

#56 covariates are left in the dataset, but I suspect many of them are correlated. Therefore, I use PCA with threshod of 90%. 
#preProc <- preProcess(training[,-dim(training)[2]], method='pca',thresh=0.90)
#preProc

#use random forest for the model fitting
modFit <- train(classe~., method="rf",preProcess="pca",data=training)

#Model Evaluation
confusionMatrix(training$classe,predict(modFit))

#out of sample error
confusionMatrix(testing$classe,predict(modFit,testing))

#prediction 
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="test.csv",method="curl")
test_pred <- read.csv("test.csv")
test_pred <- test_pred[,-col_remove]
prediction <- predict(modFit,test_pred)

write_class_predictions_to_file = function(x) {
  for (k in 1:length(x)) {
    file_name = paste0("problem_id_",k,".txt")
    write.table(x[k],file=file_name, quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

write_class_predictions_to_file(prediction)