#Logistic regression
#QTM2000, Sang Won Baek and David Shin

#Install required packages
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

#Import Bank.csv file

Bank <- read.csv("Bank.csv")

#Transform categorical variable as a factor
#Get rid of default and duration
Bank$default <- NULL
Bank$duration <- NULL

#Split data into 60% training set and 40% test set
trainSetSize <- floor(0.6 * nrow(Bank))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(Bank)), size = trainSetSize) 
bankTrainSet <- Bank[trainInd, ]               
bankTestSet <- Bank[-trainInd, ] 

#Create a logistic regression model from training data
logRegrModel <- glm(y ~ nr.employed + emp.var.rate + euribor3m + pdays, 
                    data=bankTrainSet,
                    family="binomial")

#Summarize logistic regression output 
summary(logRegrModel)  

#Exponentiated coefficients
exp(coef(logRegrModel))

#Score the logistic regression model on the test data set
predTestScores <- predict(logRegrModel, type="response", newdata=bankTestSet) 

#Create an extra column with predictions to loansTestSet, store in dfToExport 
#Export to file predictedLoans.csv
dfToExport <- data.frame(bankTestSet,predTestScores)
write.csv(dfToExport, file = "../ROutput/logRegressionBank.csv")

#Classify based on logistic regression output
##Set cutoff value 
cutoff <- 0.14
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores > cutoff] <- 1

##Output to file
dfToExport <- data.frame(bankTestSet,predTestScores,predTestClass)
write.csv(dfToExport, file = "../ROutput/logRegressionBank.csv")

#Make edits on Actual Test Class
actualTestClass <- bankTestSet$y
actualTestClass <- as.numeric(actualTestClass)-1
actualTestClass

#Create a confusion matrix
confMx <- table(actualTestClass, predTestClass) 
confMx

#Create an ROC curve
simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClass,predTestScores)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

#Create a lift chart
dfForLiftChart <- data.frame(predTestScores, actualTestClass) 
sortedData <- dfForLiftChart[order(-dfForLiftChart$predTestScores),] 
cumulCases <- cumsum(sortedData[,2]) 
##Plot the lift chart
plot(cumulCases, xlab = "Number of Cases", ylab = "Number of 1s Identified by Algorithm So Far", type="l", col = "blue") 
##Plot the 45 degree line
X <- c(0, length(predTestScores))
Y <- c(0, cumulCases[length(predTestScores)])
lines(X, Y, col = "red", type = "l", lty = 2)
