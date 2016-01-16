library(rpart)
train <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Titanic/train.csv")
test <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Titanic/test.csv")

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)

train <- train[,c(-1,-4,-9,-11)]  # passenger id, passenger name, ticket, cabin(bec. nearly empty)
train_rpart <- rpart(formula = Survived~., data=train, xval=10, cp=0.01)

library(rpart.plot)
rpart.plot(train_rpart)
printcp(train_rpart)

test.pred <- predict(train_rpart, newdata=test, type="class")

# create a new table with passengerid and the class prediction of survived / not survived
test.pred <- data.frame(PassengerId = test$PassengerId, Survived = test.pred)
write.csv(test.pred, file="titanic.rpart.prediction.csv",row.names=FALSE)
