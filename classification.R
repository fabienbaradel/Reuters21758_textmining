###############################################################
########## PREPROCESSING: CREATE TRAIN AND TEST SET ###########
###############################################################

Topics<-c("topic.earn","topic.acq","topic.money.fx","topic.grain", "topic.crude","topic.trade","topic.interest","topic.ship",
          "topic.wheat","topic.corn")

data.new<-data.frame(data[-nbRowNULL,c("pid","fileName","purpose",Topics)], lda.proba.54, bow.norm)
str(data.new)

#put topics as factors
for(i in 1:10){
  data.new[,Topics[i]]<-as.factor(data.new[,Topics[i]])
  data.new[,i+13]<-as.numeric(data.new[,i+13])
}

#split in train and test data set
rowTrain<-NA
for(i in 1:nrow(data.new)){
  if(data.new[i,"purpose"]=="train"){
    rowTrain<-c(rowTrain,i)
  }
}
rowTrain<-rowTrain[-1]
length(rowTrain)#14389

rowTest<-NA
for(i in 1:nrow(data.new)){
  if(data.new$purpose[i]=="test"){
    rowTest<-c(rowTest,i)
  }
}
rowTest<-rowTest[-1]
length(rowTest)#6079


TRAIN<-data.new[rowTrain,]#14389
TEST<-data.new[rowTest,]#6079
nrow(TRAIN)




#####################################################################################
########## SPLIT THE TRAIN SET IN ORDER TO APPLY 10 CORSS VALIDATION ################
#####################################################################################

library(randomForest)
library(e1071)
require(caret)

set.seed(42)
TRAIN<-TRAIN[sample(nrow(TRAIN)), ]

#we r gonna use 10 cross validation on train data to select the best classifier
#first, split the train set into 10 others train and test data set
#Create 10 test datasets whose lengths are equals to 1438
#For each test dataset we create a training dataset which the rest
for(i in 1:10){
  a<-1438*(i-1)+1
  b<-1438*(i)
  #create the test data set
  test<-TRAIN[a:b,]
  assign(paste("test",i,sep = ""),test)
  #create the train data set
  train<-TRAIN[-(a:b),]
  assign(paste("train",i,sep = ""),train)
}

#create a list with all the train sets
list.train<-list(train1, train2, train3, train4, train5,
                 train6, train7, train8, train9, train10)
#create a list with all the test sets
list.test<-list(test1, test2, test3, test4, test5,
                test6, test7, test8, test9, test10)



#####################################################################################
########## CREATE FUNCTIONS WHICH COMPUTE SEVERAL ACCURACIES ########################
#####################################################################################

#function which returns pourcentage of difference between two lists
accuracy<-function(data1, data2){
  nb<-sum(data1==data2)
  return(nb/(length(data1)))
}

#function which compute the total accuracy
#a row is correct if 'minTop' (max=10) the topics have been correctly assigned after prediction
accuracyTOTALminTop<-function(test, pred, minTop){
  nb<-0
  for( i in 1:nrow(pred)){
    print(i)
    if(sum(pred[i,]==test[i,4:13])<minTop){
      nb<-nb+1
    }
  }
  return(1-(nb/(nrow(pred))))
}

#accuracy total
accuracyTOT<-function(test, pred){
  acc<-0
  for(i in 1:ncol(pred)){
    acc<-acc+accuracy(test[,i+3], pred[,i])
  }
  acc<-acc/ncol(pred)
  return(acc)
}



```
Now the data is ready.

10 cross validation to see what is the best topic modeling: LDA or BAG OF WORDS
```{r}

#####################################################################################
########## CREATE FUNCTIONS WHICH PREDICT FOR BAG OF WORDS ##########################
#####################################################################################

#############################################
################## NAIVE BAYES ##############
#############################################
#function which use NAIVEBAYES to build a model on train and predict on test
#function which predict the 10 topics (hence, does 10 NB models)
#output is a matrix which 10 columns
predictNB.BOW<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  for(i in 1:10){
    print(i)
    col<-3+i
    model<-naiveBayes(train[,col]~.,data=train[,24:71], laplace = 1)
    predictions[,i]<-as.factor(predict(model, test[,24:71]))
  }
  return(predictions)
}

# 
#  a<-predictNB.BOW(train1,test1)
#  accuracyTOT(test1, a) 61.53

############################################
################## RANDOM FOREST ###########
############################################
predictRF.BOW<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  for(i in 1:10){
    print(i)
    col<-3+i
    model<-randomForest(train[,col]~.,data=train[,24:71],ntree=100)
    predictions[,i]<-as.factor(predict(model, test[,24:71]))
  }
  return(predictions)
}
# 
#  a<-predictRF.BOW(train1,test1)
#  accuracyTOT(test1, a) 97.56



###########################################
################## SVM ####################
###########################################
predictSVM.BOW<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  radial<-c(1,2,3,6)
  linear<-c(4,5,7,8,9,10)
  for(i in 1:10){
    print(i)
    col<-3+i
    model<-svm(train[,col]~.,data=train[,24:71], kernel="linear")
    predictions[,i]<-as.factor(predict(model, test[,24:71]))
  }
  return(predictions)
}

#  a<-predictSVM.BOW(train1,test1)
#  accuracyTOT(test1, a) 96.87
#  
```


10 ross validation on training set in order to see what is the best classifier
```{r}
#####################################################################################
######### CREATE FUNCTIONS WHICH PREDICT FOR LDA TOPIC MODELING #####################
#####################################################################################

##############################################
################## NAIVE BAYES ###############
##############################################
#function which use NAIVEBAYES to build a model on train and predict on test
#function which predict the 10 topics (hence, does 10 NB models)
#output is a matrix which 10 columns
predictNBlda<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  for(i in 1:10){
    print(i)
    col<-3+i
    model<-naiveBayes(train[,col]~.,data=train[,14:23], laplace = 1)
    predictions[,i]<-as.factor(predict(model, test[,14:23]))
  }
  return(predictions)
}

# 
#  a<-predictNBlda(train1,test1)
#  accuracyTOT(test1, a) 92.13




########################################
################## SVM #################
########################################
accuracies.linear<-NA
for(i in 1:10){
  print(i)
  col<-3+i
  model<-svm(train[,col]~.,data=train[,14:23], cross=10, kernel="linear")
  accuracies.linear<-c(accuracies.linear, mean(model$accuracies))
}
accuracies.linear<-accuracies.linear[-1]

accuracies.radial<-NA
for(i in 1:10){
  print(i)
  col<-3+i
  model<-svm(train[,col]~.,data=train[,14:23], cross=10, kernel="radial")
  accuracies.radial<-c(accuracies.radial, mean(model$accuracies))
}
accuracies.radial<-accuracies.radial[-1]

mean(accuracies.linear)
sd(accuracies.linear)
mean(accuracies.radial)
sd(accuracies.radial)

# we get the best accuracy with the radial kernel. #accuracies change only for class 1,2,3,6.
# so we will use radial kernel only for these topics because this kernel takes more time

#let find the gamma and the cost
#We assume that the parameteers are the same for each column, so we are going to work only with the first one
#begin with gamma
accuracy.svm.gamma<-1:6
for(i in 1:length(accuracy.svm.gamma)){
  print(i)
  accuracy.svm.gamma[i]<-mean(svm(train[,col]~.,data=train[,14:23], cross=10, kernel="radial",
                                  gamma=.09+i*0.01)$accuracies)
}
plot(x=.09+(1:6)*0.01, y=accuracy.svm.gamma, ylab="Accurancy", xlab="Gamma",
     main="Maximise accurancy to find the best gamma", type='l', ylim = c(98.646,98.6468))
max(accuracy.svm.gamma)
#doesnt matter, put equals to 0.1

#now let's find the best cost
accuracy.svm.cost<-1:6
for(i in 1:length(accuracy.svm.gamma)){
  print(i)
  accuracy.svm.cost[i]<-mean(svm(train[,col]~.,data=train[,14:23], cross=10, kernel="radial",
                                 gamma=0.1, cost=i)$accuracies)
}
plot(x=1:6, y=accuracy.svm.cost, ylab="Accurancy", xlab="Cost",
     main="Maximise accurancy to find the best cost",type = 'l', ylim = c(98.646,98.6468))
#doesnt matter neither

#we have found all the needed parameters
#
#function which use SVM to build a model on train and predict on test
#function which predict the 10 topics (hence, does 10 SVM models)
#output is a matrix which 10 columns
predictSVMlda<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  radial<-c(1,2,3,6)
  linear<-c(4,5,7,8,9,10)
  for(i in radial){
    print(i)
    col<-3+i
    model<-svm(train[,col]~.,data=train[,14:23], kernel="radial")
    predictions[,i]<-as.factor(predict(model, test[,14:23]))
  }
  for(i in linear){
    print(i)
    col<-3+i
    model<-svm(train[,col]~.,data=train[,14:23], kernel="linear")
    predictions[,i]<-as.factor(predict(model, test[,14:23]))
  }
  return(predictions)
}

#  a<-predictSVMlda(train1,test1)
#  accuracyTOT(test1, a) 96.67

####################################################
################## RANDOM FOREST ###################
####################################################
predictRFlda<-function(train, test){
  predictions <- data.frame(matrix(0, ncol = 10, nrow = nrow(test)))
  colnames(norm.vectorTEXT1.0) <- Topics
  for(i in 1:10){
    print(i)
    col<-3+i
    model<-randomForest(train[,col]~.,data=train[,14:23],ntree=100)
    predictions[,i]<-as.factor(predict(model, test[,14:23]))
  }
  return(predictions)
}
# 
#  a<-predictRFlda(train1,test1)
#  accuracyTOT(test1, a) 96.98


#####################################################################################
######### APPLY 10 CROSS VALIDATION ON TRAINING DATA: BOW & LDA #####################
#####################################################################################

#USE K-FOLD VALIDATION on train set: build the confusion matrix for the 10 topics
confusMatrixNB.bow<-matrix(0,nrow = 2, ncol = 2)
confusMatrixSVM.bow<-matrix(0,nrow = 2, ncol = 2)
confusMatrixRF.bow<-matrix(0,nrow = 2, ncol = 2)
confusMatrixNB.lda<-matrix(0,nrow = 2, ncol = 2)
confusMatrixSVM.lda<-matrix(0,nrow = 2, ncol = 2)
confusMatrixRF.lda<-matrix(0,nrow = 2, ncol = 2)
mat.bow<-matrix(nrow = 60, ncol = 3, dimnames = list(c( paste("accuracy",1:10), "average accuracy",
                                                        "std error",paste("0 precision",1:10),
                                                        "average 0 precision", "std error",
                                                        paste("0 recall",1:10), "0 average recall", "std error",
                                                        paste("1 precision",1:10),
                                                        "average 1 precision", "std error",
                                                        paste("1 recall",1:10), "1 average recall", "std error"),
                                                     c("NB", "SVM", "RF")))
mat.lda<-matrix(nrow = 60, ncol = 3, dimnames = list(c( paste("accuracy",1:10), "average accuracy",
                                                        "std error",paste("0 precision",1:10),
                                                        "average 0 precision", "std error",
                                                        paste("0 recall",1:10), "0 average recall", "std error",
                                                        paste("1 precision",1:10),
                                                        "average 1 precision", "std error",
                                                        paste("1 recall",1:10), "1 average recall", "std error"),
                                                     c("NB", "SVM", "RF")))

for(i in 1:10){#8 minutes and 20 seconds to run
  print("tour")
  print(i)
  #take the good train and tests sets from our lists
  test<-as.data.frame(list.test[i])
  train<-as.data.frame(list.train[i])
  
  ###################################################
  ################## BAG OF WORDS ###################
  ###################################################
  
  #NAIVES BAYES
  predictions<-predictNB.BOW(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.bow[i,1]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.bow[i+12,1]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.bow[i+24,1]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.bow[i+36,1]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.bow[i+48,1]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixNB.bow<-confusMatrixNB.bow+confusM
  
  #SVM
  predictions<-predictSVM.BOW(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.bow[i,2]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.bow[i+12,2]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.bow[i+24,2]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.bow[i+36,2]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.bow[i+48,2]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixSVM.bow<-confusMatrixSVM.bow+confusM
  
  #RANDOM FOREST
  predictions<-predictRF.BOW(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.bow[i,3]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.bow[i+12,3]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.bow[i+24,3]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.bow[i+36,3]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.bow[i+48,3]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixRF.bow<-confusMatrixRF.bow+confusM
  
  ###################################################
  ################## LDA TOPIC MODELS ###############
  ###################################################
  #NAIVES BAYES
  predictions<-predictNBlda(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.lda[i,1]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.lda[i+12,1]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.lda[i+24,1]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.lda[i+36,1]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.lda[i+48,1]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixNB.lda<-confusMatrixNB.lda+confusM
  
  #SVM
  predictions<-predictSVMlda(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.lda[i,2]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.lda[i+12,2]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.lda[i+24,2]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.lda[i+36,2]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.lda[i+48,2]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixSVM.lda<-confusMatrixSVM.lda+confusM
  
  #RANDOM FOREST
  predictions<-predictRFlda(train,test)
  
  #build the confusion matrix NB
  confusM<-table(as.matrix(test[,4:13]), as.matrix(predictions))
  mat.lda[i,3]<-sum(diag(confusM))/sum(confusM) #accuracy
  mat.lda[i+12,3]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #0 precision
  mat.lda[i+24,3]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #0 recall
  mat.lda[i+36,3]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #1 precision
  mat.lda[i+48,3]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #1 recall
  
  
  confusMatrixRF.lda<-confusMatrixRF.lda+confusM
  
}

#build mean and std
for(i in 1:3){
  mat.bow[11,i]<-mean(mat.bow[1:10,i])
  mat.bow[12,i]<-sd(mat.bow[1:10,i])
  mat.lda[11,i]<-mean(mat.lda[1:10,i])
  mat.lda[12,i]<-sd(mat.lda[1:10,i])
  
  mat.bow[23,i]<-mean(mat.bow[13:22,i])
  mat.bow[24,i]<-sd(mat.bow[13:22,i])
  mat.lda[23,i]<-mean(mat.lda[13:22,i])
  mat.lda[24,i]<-sd(mat.lda[13:22,i])
  
  mat.bow[35,i]<-mean(mat.bow[25:34,i])
  mat.bow[36,i]<-sd(mat.bow[25:34,i])
  mat.lda[35,i]<-mean(mat.lda[25:34,i])
  mat.lda[36,i]<-sd(mat.lda[25:34,i])
  
  mat.bow[47,i]<-mean(mat.bow[37:46,i])
  mat.bow[48,i]<-sd(mat.bow[37:46,i])
  mat.lda[47,i]<-mean(mat.lda[37:46,i])
  mat.lda[48,i]<-sd(mat.lda[37:46,i])
  
  mat.bow[59,i]<-mean(mat.bow[49:58,i])
  mat.bow[60,i]<-sd(mat.bow[49:58,i])
  mat.lda[59,i]<-mean(mat.lda[49:58,i])
  mat.lda[60,i]<-sd(mat.lda[49:58,i])
}
confusMatrixNB.bow
confusMatrixSVM.bow
confusMatrixRF.bow
mat.bow<-mat.bow*100
mat.bow
mat.lda<-mat.lda*100
mat.lda

mat.bow[1:12,]
mat.lda[1:12,]




#####################################################################
################## COMPUTE F-MEASURE & ITS DIFFERENCES ##############
#####################################################################
#create the F-measure
#label class as 0
#BOW
matF0.bow<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("0 F-measure",1:10), "average 0 F-measure",
                                                          "std error"),
                                                       c("NB", "SVM", "RF")))

matF0.bow<-(2*mat.bow[13:24,]*mat.bow[25:36,])/(mat.bow[13:24,]+mat.bow[25:36,])
for(i in 1:3){
  matF0.bow[11,i]<-mean(matF0.bow[1:10,i])
  matF0.bow[12,i]<-sd(matF0.bow[1:10,i])
}
matF0.bow

#LDA
matF0.lda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("0 F-measure",1:10), "average 0 F-measure",
                                                          "std error"),
                                                       c("NB", "SVM", "RF")))

matF0.lda<-(2*mat.lda[13:24,]*mat.lda[25:36,])/(mat.lda[13:24,]+mat.lda[25:36,])
for(i in 1:3){
  matF0.lda[11,i]<-mean(matF0.lda[1:10,i])
  matF0.lda[12,i]<-sd(matF0.lda[1:10,i])
}
matF0.lda

#label class as 1
##BOW
matF1.bow<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("1 F-measure",1:10), "average 1 F-measure",
                                                          "std error"),
                                                       c("NB", "SVM", "RF")))

matF1.bow<-(2*mat[37:48,]*mat.bow[49:60,])/(mat.bow[37:48,]+mat.bow[49:60,])
for(i in 1:3){
  matF1.bow[11,i]<-mean(matF1.bow[1:10,i])
  matF1.bow[12,i]<-sd(matF1.bow[1:10,i])
}
matF1.bow

##LDA
matF1.lda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("1 F-measure",1:10), "average 1 F-measure",
                                                          "std error"),
                                                       c("NB", "SVM", "RF")))

matF1.lda<-(2*mat.lda[37:48,]*mat.lda[49:60,])/(mat.lda[37:48,]+mat.lda[49:60,])
for(i in 1:3){
  matF1.lda[11,i]<-mean(matF1.lda[1:10,i])
  matF1.lda[12,i]<-sd(matF1.lda[1:10,i])
}
matF1.lda

#matrix of differences between BOW and LDA
matDiff.bowVSlda<-matrix(nrow = 60, ncol = 3, dimnames = list(c( paste("accuracy",1:10), "average accuracy",
                                                                 "std error",paste("0 precision",1:10),
                                                                 "average 0 precision", "std error",
                                                                 paste("0 recall",1:10), "0 average recall", "std error",
                                                                 paste("1 precision",1:10),
                                                                 "average 1 precision", "std error",
                                                                 paste("1 recall",1:10), "1 average recall", "std error"),
                                                              c("NB", "SVM", "RF")))
matFDiff0.bowVSlda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("F-measure",1:10), "0 average F-measure",
                                                                   "std error"),
                                                                c("NB", "SVM", "RF")))
matFDiff1.bowVSlda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("F-measure",1:10), "1 average F-measure",
                                                                   "std error"),
                                                                c("NB", "SVM", "RF")))


matDiff.bowVSlda[,1]<-mat.bow[,1]-mat.lda[,1]#for accuracy, precision, recall
matDiff.bowVSlda[,2]<-mat.bow[,2]-mat.lda[,2]
matDiff.bowVSlda[,3]<-mat.bow[,3]-mat.lda[,3]

matFDiff0.bowVSlda[,1]<-matF0.bow[,1]-matF0.lda[,1]#for F-measure 0
matFDiff0.bowVSlda[,2]<-matF0.bow[,2]-matF0.lda[,2]
matFDiff0.bowVSlda[,3]<-matF0.bow[,2]-matF0.lda[,3]

matFDiff1.bowVSlda[,1]<-matF1.bow[,1]-matF1.lda[,1]#for F-measure 1
matFDiff1.bowVSlda[,2]<-matF1.bow[,2]-matF1.lda[,2]
matFDiff1.bowVSlda[,3]<-matF1.bow[,3]-matF1.lda[,3]


for(i in 1:3){
  matDiff.bowVSlda[11,i]<-mean(matDiff.bowVSlda[1:10,i])#accuracy
  matDiff.bowVSlda[12,i]<-sd(matDiff.bowVSlda[1:10,i])
  matDiff.bowVSlda[23,i]<-mean(matDiff.bowVSlda[13:22,i])#precision and recall, class label as 0
  matDiff.bowVSlda[24,i]<-sd(matDiff.bowVSlda[13:22,i])
  matDiff.bowVSlda[35,i]<-mean(matDiff.bowVSlda[25:34,i])
  matDiff.bowVSlda[36,i]<-sd(matDiff.bowVSlda[25:34,i])
  matDiff.bowVSlda[23,i]<-mean(matDiff.bowVSlda[37:46,i])##precision and recall, class label as 1
  matDiff.bowVSlda[24,i]<-sd(matDiff.bowVSlda[37:46,i])
  matDiff.bowVSlda[35,i]<-mean(matDiff.bowVSlda[49:58,i])
  matDiff.bowVSlda[36,i]<-sd(matDiff.bowVSlda[49:58,i])
  matFDiff0.bowVSlda[11,i]<-mean(matFDiff0.bowVSlda[1:10,i])#F-measure class label as 0
  matFDiff0.bowVSlda[12,i]<-sd(matFDiff0.bowVSlda[1:10,i])
  matFDiff1.bowVSlda[11,i]<-mean(matFDiff1.bowVSlda[1:10,i])#F-measure class label as 1
  matFDiff1.bowVSlda[12,i]<-sd(matFDiff1.bowVSlda[1:10,i])
}


#############################################################
############ STATISTICAL TEST ON MEASURES ###################
#############################################################

#ACCURACY ANALYSIS
t.test(matDiff.bowVSlda[1:10,1])
t.test(matDiff.bowVSlda[1:10,1],alternative = "less") #there is a accuracy difference between BOW & LDA for NB
#LDA is better

t.test(matDiff.bowVSlda[1:10,2])
t.test(matDiff.bowVSlda[1:10,2],alternative = "less")
#lda is better
t.test(matDiff.bowVSlda[1:10,3])
t.test(matDiff.bowVSlda[1:10,3],alternative = "greater") #there is a accuracy difference between BOW & LDA for RF
#BOW is better

#in term of accuracy, BOW gives a better accuracy for 2 classifiers: RF & SVM

#class label as 0
#PRECISION ANALYSIS
t.test(matDiff.bowVSlda[13:22,1])
t.test(matDiff.bowVSlda[13:22,1],alternative = "greater") #there is a difference between BOW & LDA for NB
#LDA is better
t.test(matDiff.bowVSlda[13:22,2])
t.test(matDiff.bowVSlda[13:22,2],alternative = "greater") #there is a difference between BOW & LDA for NB
#BOW is better

t.test(matDiff.bowVSlda[13:22,3])
t.test(matDiff.bowVSlda[13:22,3],alternative = "greater") #there is a difference between BOW & LDA for NB
#BOW is better

#in term of precision, BOW gives a better accuracy for 2 classifiers: RF & SVM

#RECALL ANALYSIS
t.test(matDiff.bowVSlda[25:34,1])
t.test(matDiff[25:34,1],alternative = "less") #there is a difference between BOW & LDA for NB
#LDA is better

t.test(matDiff.bowVSlda[25:34,2])
t.test(matDiff.bowVSlda[25:34,2],alternative = "less") #there is a difference between BOW & LDA for NB
#LDA is better

t.test(matDiff.bowVSlda[25:34,3])
t.test(matDiff.bowVSlda[25:34,3],alternative = "greater") #there is a difference between BOW & LDA for NB
#BOW is better


#in term of recall,  LDA gives a better accuracy for 2 classifiers: NB & SVM

#F-MEASURE ANALYSIS
t.test(matFDiff0.bowVSlda[1:10,1])
t.test(matFDiff0.bowVSlda[1:10,1],alternative = "less")
#LDA is better

t.test(matFDiff0.bowVSlda[1:10,2])
t.test(matFDiff0.bowVSlda[1:10,2],alternative = "greater")
#BOW is better

t.test(matFDiff0.bowVSlda[1:10,3])
t.test(matFDiff0.bowVSlda[1:10,3],alternative = "less") 
#LDA is better



#class label as 1
#PRECISION ANALYSIS
t.test(matDiff.bowVSlda[13:22,1])
t.test(matDiff.bowVSlda[13:22,1],alternative = "greater") 
#BOW is better

t.test(matDiff.bowVSlda[13:22,2])
t.test(matDiff.bowVSlda[13:22,2],alternative = "greater") 
#BOW is better

t.test(matDiff.bowVSlda[13:22,3])
t.test(matDiff.bowVSlda[13:22,3],alternative = "greater")
#BOW is better



#RECALL ANALYSIS
t.test(matDiff.bowVSlda[25:34,1])
t.test(matDiff.bowVSlda[25:34,1],alternative = "less") 
#LDA is better

t.test(matDiff.bowVSlda[25:34,2])
t.test(matDiff.bowVSlda[25:34,2],alternative = "less")
#LDA is better

t.test(matDiff.bowVSlda[25:34,3])
t.test(matDiff.bowVSlda[25:34,3],alternative = "greater") 
#BOW BETTER

#F-MEASURE ANALYSIS
t.test(matFDiff1.bowVSlda[1:10,1])
t.test(matFDiff1.bowVSlda[1:10,1],alternative = "greater")
#bow better

t.test(matFDiff1.bowVSlda[1:10,2])
#SAME

t.test(matFDiff1.bowVSlda[1:10,3])
#same

#we r not gonna compute the micro average for recall, precion because they are he same as average in this case



#############################################################
############ COMPUTE MACRO-AVERAGES  & DIFF #################
#############################################################



#compute the MICRO-average
matMacro.bow<-matrix(nrow = 24, ncol = 3, dimnames = list(c( paste("macro precision",1:10), " average macro precision",
                                                             "std error",paste("macro recall",1:10),
                                                             "average macro recall", "std error"),
                                                          c("NB", "SVM", "RF")))
matMacro.lda<-matrix(nrow = 24, ncol = 3, dimnames = list(c( paste("macro precision",1:10), " average macro precision",
                                                             "std error",paste("macro recall",1:10),
                                                             "average macro recall", "std error"),
                                                          c("NB", "SVM", "RF")))
for(i in 1:10){
  matMacro.bow[i,]<-(mat.bow[12+i,]+mat.bow[36+i,])/2#macro precision
  matMacro.bow[12+i,]<-(mat.bow[24+i,]+mat.bow[48+i,])/2#macro recall
  matMacro.lda[i,]<-(mat.lda[12+i,]+mat.lda[36+i,])/2#macro precision
  matMacro.lda[12+i,]<-(mat.lda[24+i,]+mat.lda[48+i,])/2#macro recall
}

for(i in 1:3){
  matMacro.bow[11,i]<-mean(matMacro.bow[1:10,i])#mean and sd for macro precision
  matMacro.bow[12,i]<-sd(matMacro.bow[1:10,i])
  matMacro.bow[23,i]<-mean(matMacro.bow[13:22,i])#mean and sd for macro recall
  matMacro.bow[24,i]<-sd(matMacro.bow[13:22,i])
  
  matMacro.lda[11,i]<-mean(matMacro.lda[1:10,i])#mean and sd for macro precision
  matMacro.lda[12,i]<-sd(matMacro.lda[1:10,i])
  matMacro.lda[23,i]<-mean(matMacro.lda[13:22,i])#mean and sd for macro recall
  matMacro.lda[24,i]<-sd(matMacro.lda[13:22,i])
}
matMacro.bow
matMacro.lda

#compute the macro F-measure
matMacroF.bow<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("macro F-measure",1:10), " average F-measure",
                                                              "std error"),
                                                           c("NB", "SVM", "RF")))
matMacroF.lda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("macro F-measure",1:10), " average F-measure",
                                                              "std error"),
                                                           c("NB", "SVM", "RF")))

for(i in 1:10){
  matMacroF.bow[i,]<-(2*matMacro.bow[i,]*matMacro.bow[12+i,])/(matMacro.bow[i,]+matMacro.bow[12+i,])#macro Fmeasure bow
  matMacroF.lda[i,]<-(2*matMacro.lda[i,]*matMacro.lda[12+i,])/(matMacro.lda[i,]+matMacro.lda[12+i,])#macro Fmeasure lda
}

for(i in 1:3){
  matMacroF.bow[11,i]<-mean(matMacroF.bow[1:10,i])#mean and sd for macro F-measure bow
  matMacroF.bow[12,i]<-sd(matMacroF.bow[1:10,i])
  matMacroF.lda[11,i]<-mean(matMacroF.lda[1:10,i])#mean and sd for macro F-measure lda
  matMacroF.lda[12,i]<-sd(matMacroF.lda[1:10,i])
}
matMacroF.bow
matMacroF.lda


#build the F-measure difference and use statistical tests
matMacroFDiff.bowVSlda<-matrix(nrow = 12, ncol = 3, dimnames = list(c( paste("macro F-measure",1:10), " average F-measure",
                                                                       "std error"),
                                                                    c("NB", "SVM", "RF")))

matMacroFDiff.bowVSlda[,1]<-matMacroF.bow[,1]-matMacroF.lda[,1]
matMacroFDiff.bowVSlda[,2]<-matMacroF.bow[,2]-matMacroF.lda[,2]
matMacroFDiff.bowVSlda[,3]<-matMacroF.bow[,3]-matMacroF.lda[,3]

for(i in 1:3){
  matMacroFDiff.bowVSlda[11,i]<-mean(matMacroFDiff.bowVSlda[1:10,i])#mean and sd for macro F-measure difference
  matMacroFDiff.bowVSlda[12,i]<-sd(matMacroFDiff.bowVSlda[1:10,i])
}
matMacroFDiff.bowVSlda


#############################################################
############ STAT TEST ON MICRO-AVERAGES ####################
#############################################################

t.test(matMacroFDiff.bowVSlda[1:10,1])
t.test(matMacroFDiff.bowVSlda[1:10,1],alternative = "less")
#LDA IS BETTER

t.test(matMacroFDiff.bowVSlda[1:10,2])
t.test(matMacroFDiff.bowVSlda[1:10,2],alternative = "greater")
#BOW BETTER

t.test(matMacroFDiff.bowVSlda[1:10,3])
t.test(matMacroFDiff.bowVSlda[1:10,3],alternative = "greater")
#BOW BETTER


###################################################################################
###### FIND THE BEST CLASSIFIER INTO RF BAG OF WORDS and SVM LDA ##################
###################################################################################


#matrix of differences
matDiff<-matrix(nrow = 60, ncol = 1, dimnames = list(c( paste("accuracy",1:10), "average accuracy",
                                                        "std error",paste("0 precision",1:10),
                                                        "average 0 precision", "std error",
                                                        paste("0 recall",1:10), "0 average recall", "std error",
                                                        paste("1 precision",1:10),
                                                        "average 1 precision", "std error",
                                                        paste("1 recall",1:10), "1 average recall", "std error"),
                                                     c("RF BOW - SVM LDA")))
matFDiff0<-matrix(nrow = 12, ncol = 1, dimnames = list(c( paste("F-measure",1:10), "0 average F-measure",
                                                          "std error"),
                                                       c("RF BOW - SVM LDA")))
matFDiff1<-matrix(nrow = 12, ncol = 1, dimnames = list(c( paste("F-measure",1:10), "1 average F-measure",
                                                          "std error"),
                                                       c("RF BOW - SVM LDA")))


matDiff[,1]<-mat.bow[,3]-mat.lda[,2]#for accuracy, precision, recall


matFDiff0[,1]<-matF0.bow[,3]-matF0.lda[,2]#for F-measure 0


matFDiff1[,1]<-matF1.bow[,3]-matF1.lda[,2]#for F-measure 1



for(i in 1){
  matDiff[11,i]<-mean(matDiff[1:10,i])#accuracy
  matDiff[12,i]<-sd(matDiff[1:10,i])
  matDiff[23,i]<-mean(matDiff[13:22,i])#precision and recall, class label as 0
  matDiff[24,i]<-sd(matDiff[13:22,i])
  matDiff[35,i]<-mean(matDiff[25:34,i])
  matDiff[36,i]<-sd(matDiff[25:34,i])
  matDiff[47,i]<-mean(matDiff[37:46,i])##precision and recall, class label as 1
  matDiff[48,i]<-sd(matDiff[37:46,i])
  matDiff[59,i]<-mean(matDiff[49:58,i])
  matDiff[60,i]<-sd(matDiff[49:58,i])
  matFDiff0[11,i]<-mean(matFDiff0[1:10,i])#F-measure class label as 0
  matFDiff0[12,i]<-sd(matFDiff0[1:10,i])
  matFDiff1[11,i]<-mean(matFDiff1[1:10,i])#F-measure class label as 1
  matFDiff1[12,i]<-sd(matFDiff1[1:10,i])
}

#ACCURACY ANALYSIS
t.test(matDiff[1:10,1])
t.test(matDiff[1:10,1],alternative = "greater") 
#rf better


#class label as 0
#PRECISION ANALYSIS
t.test(matDiff[13:22,1])
t.test(matDiff[13:22,1],alternative = "greater") 
#rf better

#RECALL ANALYSIS
t.test(matDiff[25:34,1])
t.test(matDiff[25:34,1],alternative = "less")
#svm better


#F-MEASURE ANALYSIS
t.test(matFDiff0[1:10,1])
t.test(matFDiff0[1:10,1],alternative = "greater")
#rf better



#class label as 1
#PRECISION ANALYSIS
t.test(matDiff[13:22,1])
t.test(matDiff[13:22,1],alternative = "greater") 
#rf better

#RECALL ANALYSIS
t.test(matDiff[25:34,1])
t.test(matDiff[25:34,1],alternative = "less")
#svm better


#F-MEASURE ANALYSIS
t.test(matFDiff1[1:10,1])
t.test(matFDiff1[1:10,1],alternative = "greater")
#svm better


#in term of f-measure, we need to take RF! 2nd SVM, and 3th NB

#we r not gonna compute the micro average for recall, precion because they are he same as average in this case

#compute the MACRO-average
matMacro<-matrix(nrow = 24, ncol = 2, dimnames = list(c( paste("macro precision",1:10), " average macro precision",
                                                         "std error",paste("macro recall",1:10),
                                                         "average macro recall", "std error"),
                                                      c("RF BOW","SVM LDA")))
#RF BOW
for(i in 1:10){
  matMacro[i,1]<-(mat.bow[12+i,3]+mat.bow[36+i,3])/2#macro precision
  matMacro[12+i,1]<-(mat.bow[24+i,3]+mat.bow[48+i,3])/2#macro recall
}

#svm lda
for(i in 1:10){
  matMacro[i,2]<-(mat.lda[12+i,2]+mat.lda[36+i,2])/2#macro precision
  matMacro[12+i,2]<-(mat.lda[24+i,2]+mat.lda[48+i,2])/2#macro recall
}

for(i in 1:2){
  matMacro[11,i]<-mean(matMacro[1:10,i])#mean and sd for macro precision
  matMacro[12,i]<-sd(matMacro[1:10,i])
  matMacro[23,i]<-mean(matMacro[13:22,i])#mean and sd for macro recall
  matMacro[24,i]<-sd(matMacro[13:22,i])
}
matMacro

#compute the MACRO-average difference
matMacroDiff<-matrix(nrow = 24, ncol = 1, dimnames = list(c( paste("macro precision",1:10), " average macro precision",
                                                             "std error",paste("macro recall",1:10),
                                                             "average macro recall", "std error"),
                                                          c("RF BOW - SVM LDA")))

matMacroDiff[,1]<-matMacro[,1]-matMacro[,2]

for(i in 1:2){
  matMacroDiff[11,i]<-mean(matMacroDiff[1:10,i])#mean and sd for macro precision diff
  matMacroDiff[12,i]<-sd(matMacroDiff[1:10,i])
  matMacroDiff[23,i]<-mean(matMacroDiff[13:22,i])#mean and sd for macro recall diff
  matMacroDiff[24,i]<-sd(matMacroDiff[13:22,i])
}
matMacroDiff


t.test(matMacroDiff[1:10,1])
t.test(matMacroDiff[1:10,1],alternative = "less") 
#svm better for precsion

t.test(matMacroDiff[13:22,1])
t.test(matMacroDiff[13:22,1],alternative = "greater") 
#rf better for precsion

#compute the macro F-measure
matMacroF<-matrix(nrow = 12, ncol = 2, dimnames = list(c( paste("macro F-measure",1:10), " average F-measure",
                                                          "std error"),
                                                       c("RF BOW","SVM LDA")))

for(i in 1:10){
  matMacroF[i,]<-(2*matMacro[i,]*matMacro[12+i,])/(matMacro[i,]+matMacro[12+i,])#macro Fmeasure
}

for(i in 1:2){
  matMacroF[11,i]<-mean(matMacroF[1:10,i])#mean and sd for macro F-measure
  matMacroF[12,i]<-sd(matMacroF[1:10,i])
}
matMacroF

#build the F-measure difference and use statistical tests
matMacroFDiff<-matrix(nrow = 12, ncol = 1, dimnames = list(c( paste("macro F-measure",1:10), " average F-measure",
                                                              "std error"),
                                                           c("RF BOW - SVM LDA")))

matMacroFDiff[,1]<-matMacroF[,1]-matMacroF[,2]


matMacroFDiff[11,1]<-mean(matMacroFDiff[1:10,1])#mean and sd for macro F-measure difference
matMacroFDiff[12,1]<-sd(matMacroFDiff[1:10,1])

matMacroFDiff

t.test(matMacroFDiff[1:10,1])
t.test(matMacroFDiff[1:10,1],alternative = "greater") 
#rf better

#############################################################################################
################# PREDICT THE TEST SET USING THE BEST MODEL: RF ON BOW ######################
#############################################################################################

#RANDOM FOREST on TEST set using BOW
predictionsRF.bow<-predictRF.BOW(TRAIN,TEST)
accuracyTOT(TEST, predictionsRF.bow)#96.85

#RANDOM FOREST on TEST set using LDA
predictionsRF.lda<-predictRFlda(TRAIN,TEST)
accuracyTOT(TEST, predictionsRF.lda)#96.59

#SVM on TEST set using LDA
predictionsSVM.lda<-predictSVMlda(TRAIN,TEST)
accuracyTOT(TEST, predictionsSVM.lda)#96.57

#SVM on TEST set using BOW
predictionsSVMbow<-predictSVM.BOW(TRAIN,TEST)
accuracyTOT(TEST, predictionsSVMbow)#96.48

#The random forest using BOW is the best classifier on the TEST SET! we were right!


############################################################
############### FOCUS ON RF USING BOW ######################
############################################################

#WE ARE GOING TO FOCUS ON RF USING BOW NOW!
#see the accuracy per topic
accTOPICSRF<-matrix(nrow = 1, ncol = 10, dimnames = list("accuracy",Topics))
for(i in 1:10){
  accTOPICSRF[1,i]<-accuracy(predictionsRF.bow[,i],TEST[,3+i])
}
accTOPICSRF

#see if we get better accuracy in claas with svm
accTOPICSSVM<-matrix(nrow = 1, ncol = 10, dimnames = list("accuracy",Topics))
for(i in 1:10){
  accTOPICSSVM[1,i]<-accuracy(predictionsSVM.lda[,i],TEST[,3+i])
}
accTOPICSSVM

#we get better accuracy with SVM.lda in earn
#maybe useful to run a svm on these classes in order to get a higher accuracy


#see the accuracy to predict exactly the 1:10 topics (1 or 0)
acc1AT10<-1:10
for(i in 1:10){
  print(i)
  acc1AT10[i]<-accuracyTOTALminTop(test = TEST, pred = predictionsRF.bow, minTop = i)
}
acc1AT10
plot(x=6:10, y=acc1AT10[6:10], ylab="Accuracy", xlab="Number of correct predicted topics", type ='o',
     main="Accuracy to correctly predict k topics for each document")
#we get accuracy equals to 72.27% to predict correctly the 10 topics for the whole TEST set
#i.e forecast the WHOLE TEST set

####to run
#################################################################
############### SUMMARY ON AVERAGE ##############################
#################################################################

#build the confusion matrix
confusM<-table(as.matrix(TEST[,4:13]), as.matrix(predictionsRF.bow))
sum(diag(confusM))/sum(confusM) #accuracy

#0 as label factor
recall0<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #recall (0 is the initial factor)
recall0#99.00%
precision0<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #precision
precision0#97.71%
Fmeasure0<-(2*precision0*recall0)/(recall0+precision0)
Fmeasure0#98.36%

#1 as label factor
recall1<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #recall
recall1#50.77%
precision1<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #precision
precision1#70.95%
Fmeasure1<-(2*precision1*recall1)/(recall1+precision1)
Fmeasure1#59.19%

#macro-average
macroPrecision<-0.5*(precision1+precision0)
macroPrecision#84.33%
macroRecall<-0.5*(recall1+recall0)
macroRecall#74.89%
macroFmeasure<-(2*macroPrecision*macroRecall)/(macroRecall+macroPrecision)
macroFmeasure#79.33%

#################################################################
############### SUMMARY MATRIX FOR EACH TOPIC ###################
#################################################################
summaryRF<-matrix(nrow = 10, ncol = 11, dimnames = list(c("accuracy","precison 0","recall 0","F measure 0",
                                                          "precison 1","recall 1","F measure 1",
                                                          "macro precision","macro recall","macro F measure"),
                                                        c(Topics,"Average")))

for(i in 1:10){
  confusMTopic<-table(as.matrix(TEST[,3+i]), as.matrix(predictionsRF.bow[,i]))
  summaryRF[1,i]<-sum(diag(confusMTopic))/sum(confusMTopic) #accuracy
  
  #0 as label factor
  summaryRF[2,i]<-confusMTopic[1,1]/(confusMTopic[1,1]+confusMTopic[2,1]) #precision
  summaryRF[3,i]<-confusMTopic[1,1]/(confusMTopic[1,1]+confusMTopic[1,2]) #recall
  summaryRF[4,i]<-(2*summaryRF[3,i]*summaryRF[2,i])/(summaryRF[3,i]+summaryRF[2,i]) #FM
  
  #1 as label factor
  summaryRF[5,i]<-confusMTopic[2,2]/(confusMTopic[2,2]+confusMTopic[1,2]) #precision
  summaryRF[6,i]<-confusMTopic[2,2]/(confusMTopic[2,2]+confusMTopic[2,1]) #recall
  summaryRF[7,i]<-(2*summaryRF[5,i]*summaryRF[6,i])/(summaryRF[5,i]+summaryRF[6,i]) #FM
  
  #micro-average
  summaryRF[8,i]<-0.5*(summaryRF[2,i]+summaryRF[5,i])
  summaryRF[9,i]<-0.5*(summaryRF[3,i]+summaryRF[6,i])
  summaryRF[10,i]<-(2*summaryRF[8,i]*summaryRF[9,i])/(summaryRF[8,i]+summaryRF[9,i])
}
#for the average
summaryRF[1,11]<-sum(diag(confusM))/sum(confusM) #accuracy

#0 as label factor
summaryRF[2,11]<-confusM[1,1]/(confusM[1,1]+confusM[2,1]) #prec11sion
summaryRF[3,11]<-confusM[1,1]/(confusM[1,1]+confusM[1,2]) #recall
summaryRF[4,11]<-(2*summaryRF[3,11]*summaryRF[2,11])/(summaryRF[3,11]+summaryRF[2,11]) #FM

#1 as label factor
summaryRF[5,11]<-confusM[2,2]/(confusM[2,2]+confusM[1,2]) #prec11s11on
summaryRF[6,11]<-confusM[2,2]/(confusM[2,2]+confusM[2,1]) #recall
summaryRF[7,11]<-(2*summaryRF[5,11]*summaryRF[6,11])/(summaryRF[5,11]+summaryRF[6,11]) #FM

#micro-average
summaryRF[8,11]<-0.5*(summaryRF[2,11]+summaryRF[5,11])
summaryRF[9,11]<-0.5*(summaryRF[3,11]+summaryRF[6,11])
summaryRF[10,11]<-(2*summaryRF[8,11]*summaryRF[9,11])/(summaryRF[8,11]+summaryRF[9,11])  

summaryRF<-summaryRF*100
summaryRF

#explain why we get some recall and precision for 1 close to 0.
#....show few example
i<-8
table(as.matrix(TEST[,3+i]), as.matrix(predictionsRF.bow[,i]))
#because there are not a lot of 1 in the test set so there is a huge volatility