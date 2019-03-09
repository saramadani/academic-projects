library(shiny)
library(shinythemes)
library(dplyr)
library(questionr)
library(rpart)
library(rpart.plot)
library(nnet)
library(NeuralNetTools)
library(kernlab)
library(ROCR)
library(pROC)
library(plotly)
setwd(getwd())

german_data <- read.csv("/home/saara/Desktop/shiny_german/german credit dataset.csv")



attach(german_data)
german_data$Good[Good == 2] = 0

#####################################
#Preparing models for comparison #
#####################################
german_data$Good = as.factor(german_data$Good)

train_id=sample(1000,700)
data_train=german_data[train_id,]
data_test=german_data[-train_id,]
logit=glm(Good~.,data = data_train,family = binomial)


prob_test=predict(logit,data_test,type = "response")
pred_test=prediction(prob_test,data_test$Good)
perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")

mycontrol = rpart.control(cp = 0, xval = 10)
model_ar = rpart(Good~ .,method = "class" ,control = mycontrol, data=data_train)
prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
pred_test_ar=prediction(prob_test_ar,data_test$Good)
perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")

Neural = nnet(Good~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
fitNeural = predict(Neural,newdata=data_test)
prednn = prediction( fitNeural, data_test$Good)
perfnn = performance(prednn, "tpr", "fpr")
######################################################
