#QTM2000 Case Studies in Business Analytics
#Running a decision tree model to predict subscribers based on
#the input variables
#Estimating model performance
#Copyright (c) 2017 by Sang Won Baek and David Shin

#import necessary libraries
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

if (!require("rpart")) {
  install.packages("rpart")
  library("rpart")
}
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}

Bank <- read.csv("Bank.csv", header=TRUE, sep=",")
a
#Clean data: transform all categorical variables into factors;
Bank$default <- NULL
Bank$duration <- NULL

#Split data into 60% training set and 40% test set
trainSetSize <- floor(0.6 * nrow(Bank))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(Bank)), size = trainSetSize) 
BankTrainSet <- Bank[trainInd, ]               
BankTestSet <- Bank[-trainInd, ] 

dfToExport <- data.frame(BankTrainSet)
write.csv(dfToExport, file = "../ROutput/BankTrainSetData.csv")

BankTrainSet <- read.csv("BankTrainSetData1.csv", header=TRUE, sep=",")
#Create the full classification tree
fullTree <- rpart(y ~ emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m,
                  data=BankTrainSet,
                  method = "class",
                  control = rpart.control(minsplit = 1,cp = 0))
#rpart.plot(fullTree, type=3, extra=1)
printcp(fullTree)

#Prune the tree
prunedTree <- prune(fullTree, cp=.01)

#rpart.plot(prunedTree, type=4, extra=1)
printcp(prunedTree)
rpart.plot(prunedTree, type=3, extra=1)

#Fit the rules to new data (test set)
pred <- predict(prunedTree, newdata = BankTestSet, type="class")
confMx <- table(actual=BankTestSet$y,
                predicted=pred)
addmargins(confMx)
prop.table(confMx) 