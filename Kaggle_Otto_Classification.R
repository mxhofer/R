# Import both the training and test data
train<-read.csv("~/Desktop/R/train.csv")
test<-read.csv("~/Desktop/R/test.csv")

# Cut the first column, id, because R would otherwise use it as an attribute.
train1<-train[,-1]

# Create an rpart file with a very low cp value in order to find the optimal rel error.
train1.rpart <- rpart (target~., data=train1, xval=10, cp=0.000001)

# Print the cp value table
printcp(train1.rpart)

# Plot the cp value against the rel error
plotcp(train1.rpart,upper = "splits")

# Take the corresponding cp value, i.e. the left-most point below the line representing one stdev, and cut it there
prune(train1.rpart, cp = 1.4570e-04)

# Create a new table, taking the training data parameters to predict the test data classes
test.pred<-predict(train1.rpart, newdata=test, type="prob")

# Add the id value, which represents the product
test.pred = data.frame(1:nrow(test.pred),test.pred)
names(test.pred) = c('id', paste0('Class_',1:9))
