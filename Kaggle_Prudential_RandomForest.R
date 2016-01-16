install.packages("randomForest")
require(randomForest)

train <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/train.csv")
test <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/test.csv")

train <- train[, -1] #cut the first column, id
test <- test[, -1] #cut the first column, id

pmiss <- function (x){ #create function to 
  sum(is.na(x)/length(x)*100)
}

for (i in 1:ncol(train)){ #loop through all values in a column and return its index if there's a missing value 
  print(c (i, pmiss(train[,i])))
}

train.random <- train[,-c(12,15,17,29,34,35,36,37,38,47,52,61,69)] #cut columns with missing values
test.random <- test[,-c(12,15,17,29,34,35,36,37,38,47,52,61,69)] #cut columns with missing values

train.random$Response <- as.factor(train.random$Response) #set Response column to type factor

#use randomForest 
pru.random <- randomForest(Response~., ntree = 80, data = train.random) #create randomForest model

mysub <- predict(pru.random, test.random, type ="class")
test <- read.csv("~maximilianhofer/Dropbox/UCL/Year 2/Scenario Week 3 January/Kaggle Prudential/test.csv") #rerun to add id column
mysubdata <- data.frame(Id=test$Id, Response=mysub) #create submission file

write.csv(mysubdata, file="prudential.prediction.randomforest.csv",row.names=FALSE) #write submission file
