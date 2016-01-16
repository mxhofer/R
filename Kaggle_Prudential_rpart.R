library(rpart)
train <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/train.csv")
test <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/test.csv")

train <- train[, -1] #cut the first column, id
test <- test[, -1] #cut the first column, id

train$Response <- as.factor(train$Response) #make Response column factor
train.rpart <- rpart (Response~., data=train, xval=10, cp=0.000001) #create rpart model

test.pred <- predict(train.rpart, newdata=test, type="class") #predict values on the test set
test.pred <- matrix(test.pred,19765,1) #create empty matrix
test.pred <- cbind(test.pred, test$Id) 

#Reload to include the Id column
train <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/train.csv")
mysub <- data.frame(Id=test$Id, Response=test.pred[,1]) #create submission file with Id and Response columns

rpart.plot(train.rpart)

write.csv(mysub, file="prudential.prediction.csv",row.names=FALSE)
