setwd("D:/DigitRecognition")
getwd()

#install.packages("devtools")
#install_github("davpinto/fastknn")

library(devtools)
library(caTools)
library(fastknn)


train<-read.csv("train.csv")
test<-read.csv("test.csv")
#not checking for out of range values as it does not make sense???

#function to plot the digit
plotImage <- function(x) {
m = matrix(unlist(train[x,-1]), nrow = 28, byrow = TRUE)
image(m,col=grey.colors(255))
}
#plotting the image on the 10th row
plotImage(10)
#image needs to be rotated to the right
#checking other images
plotImage(1)
plotImage(50)
plotImage(100)
plotImage(300)

#all the images need to be rotated to the right to be viewed properly
#new plot image function
# reverses (rotates the matrix)
rotate <- function(x) t(apply(x, 2, rev))
newPlotImage <- function(x) {
       image(
           rotate(matrix(unlist(train[x,-1]),nrow = 28, byrow = TRUE)),
           col=grey.colors(255),
           xlab=train[x,1]
         )
}
newPlotImage(10)
newPlotImage(1)
newPlotImage(50)
newPlotImage(100)
newPlotImage(300)

#Image can now be viewed correctly
#splitting the train into holdout sample for validation

split<-sample.split(train,SplitRatio = 0.75)
splitTrain<-train[split,]
splitTest<-train[!split,]

#Extracting the labels of the split training classes
splitTrainLabels<-splitTrain[,1]
splitTestLabels<-splitTest[,1]

#as per thumb rule, square root of number of observations as initial value for k
sqrt(nrow(splitTrain))
pred<-fastknn(as.matrix(splitTrain),as.factor(splitTrainLabels),as.matrix(splitTest),k=177,method = "vote")
#accuracy
sprintf("Accuracy: %.2f", 100 * (1 - classLoss(actual = as.factor(splitTestLabels), predicted = pred$class)))
table(Actual=splitTestLabels,predicted=pred$class)

#finding best k for knn
k<-fastknnCV(as.matrix(splitTrain),as.factor(splitTrainLabels), k = 5:15, method = "vote", folds = 5, eval.metric = "auc")

pred<-fastknn(as.matrix(splitTrain),as.factor(splitTrainLabels),as.matrix(splitTest),k=k$best_k,method = "vote")
sprintf("Accuracy: %.2f", 100 * (1 - classLoss(actual = as.factor(splitTestLabels), predicted = pred$class)))
table(Actual=splitTestLabels,predicted=pred$class)
#accuracy improved from 91% to 95.8%

#using k=15 to predict the labels for test data
xLabel<-train[,1]
pred<-fastknn(as.matrix(train[,-1]),as.factor(xLabel),as.matrix(test),k=k$best_k,method = "vote")
sampleSub<-read.csv("sample_submission.csv")
sampleSub$Label<-pred$class
write.csv(sampleSub,file = 'submission1.csv',row.names = TRUE)
