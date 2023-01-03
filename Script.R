
##########################################################
# 1-Import data and install packages 
##########################################################

# install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

if(!require(devtools)) install.packages("devtools")
library(devtools)

if(!require(gdata)) install.packages("gdata")
library(gdata)

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

if(!require(rpart)) install.packages("rpart")
library(rpart)

if(!require(rpart.plot)) install.packages("rpart.plot ")
library(rpart.plot)


if(!require(randomForest)) install.packages("randomForest")
library(randomForest)

if(!require(magrittr)) install.packages("cran.r-project.org/src/contrib/Archive/magrittr/…", repos = NULL, type="source")
library(magrittr)

if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

if(!require(corrplot)) install.packages("corrplot ")
library(corrplot)

# import data
data_url<-"https://raw.githubusercontent.com/BN33/TelcoChurnProject-HarvardX-PH125.9x-Data-Science-Capstone-/main/WA_Fn-UseC_-Telco-Customer-Churn.csv"
data_original<-read.csv(data_url)

##########################################################
# 2-Data visualisation and modeling 
##########################################################

## 2.2 Datasets overview 

### 2.2.1 Overview of the original dataset

# main indicators
dim(data_original)[1]
dim(data_original)[2]

# dataset overview
str(data_original)

# outcome to predict
churn_summary <-data_original %>% group_by(Churn) %>% summarise(Count=n()) %>% mutate(percentage=Count/sum(Count))
churn_summary

round(churn_summary[2,3],4)*100

### 2.2.2 Overview of the continuous predictors

# overview of continuous predictors
continuous <-select_if(data_original, is.numeric)
summary(continuous)

# visualisation of the continuous predictors

# set the colors to use (in the whole report)
colors=c("#EEF5DB" , "#FE5F55" , "#B8D8D8" , "#7A9E9F" , "#4F6367" , "#203539" , "#D2E49F" , "#B1C47F" , "#BE1A12")

# select continuous features
continous_names<-colnames(continuous)

# build the boxplots
boxplot1<- continuous %>% ggplot(aes(x= SeniorCitizen)) + geom_boxplot(fill = colors[3], colour = colors[6])+coord_flip()+theme(panel.background = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") + facet_grid(. ~ continous_names[1])

boxplot2<- continuous %>% ggplot(aes(x= tenure)) + geom_boxplot(fill = colors[3], colour = colors[6])+coord_flip()+theme(panel.background = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") + facet_grid(. ~ continous_names[2])

boxplot3<- continuous %>% ggplot(aes(x= MonthlyCharges)) + geom_boxplot(fill = colors[3], colour = colors[6])+coord_flip()+theme(panel.background = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") + facet_grid(. ~ continous_names[3])

boxplot4<- continuous %>% ggplot(aes(x= TotalCharges)) + geom_boxplot(fill = colors[3], colour = colors[6])+coord_flip()+theme(panel.background = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("") + facet_grid(. ~ continous_names[4])

# arrange and print the bowplots
ggarrange(boxplot1,boxplot2,boxplot3,boxplot4)


# correlation mapping
corrplot(cor(na.omit(continuous)), method="color", col=colorRampPalette(c(colors[5],"white",colors[2]))(200) )

# Correlation between Tenure and Total Charges
round(cor(na.omit(data_original)$tenure,na.omit(data_original)$TotalCharges),2)

# Correlation between Monthly Charges and Total Charges
round(cor(na.omit(data_original)$MonthlyCharges,na.omit(data_original)$TotalCharges),2)

# Correlation between Total Charges and the Tenure multiplied by the Monthly Charges
round(cor( na.omit(data_original)$MonthlyCharges* na.omit(data_original)$tenure ,na.omit(data_original)$TotalCharges),4)

# Tenure summary
summary(continuous)[1,2]
summary(continuous)[5,2]
summary(continuous)[3,2]

# Monthly charges summary
summary(continuous)[1,3]
summary(continuous)[5,3]
summary(continuous)[3,3]

# Total charges summary
summary(continuous)[1,4]
summary(continuous)[5,4]
summary(continuous)[3,4]


### 2.2.3 Overview of the categorical predictors

# visualisation of the categorical predictors

# prepare the summary tables per feature
gender <- data_original %>% group_by(Value=gender) %>% summarise(Count=n())
Partner <- data_original %>% group_by(Value=Partner) %>% summarise(Count=n())
Dependents <- data_original %>% group_by(Value=Dependents) %>% summarise(Count=n())
PhoneService <- data_original %>% group_by(Value= PhoneService) %>% summarise(Count =n())
MultipleLines <- data_original %>% group_by(Value= MultipleLines) %>% summarise(Count =n())
InternetService <- data_original %>% group_by(Value=InternetService) %>% summarise(Count=n())
OnlineSecurity <- data_original %>% group_by(Value=OnlineSecurity) %>% summarise(Count =n())
OnlineBackup <- data_original %>% group_by(Value=OnlineBackup) %>% summarise(Count =n())
DeviceProtection <- data_original %>% group_by(Value=DeviceProtection) %>% summarise(Count=n())
TechSupport <- data_original %>% group_by(Value=TechSupport) %>% summarise(Count=n())
StreamingTV <- data_original %>% group_by(Value=StreamingTV) %>% summarise(Count=n())
StreamingMovies <- data_original %>% group_by(Value=StreamingMovies) %>% summarise(Count=n())
Contract <- data_original %>% group_by(Value= Contract) %>% summarise(Count=n())
PaperlessBilling <- data_original %>% group_by(Value=PaperlessBilling) %>% summarise(Count=n())
PaymentMethod <- data_original %>% group_by(Value=PaymentMethod) %>% summarise(Count=n())

# combine them in a first table and build the associated chart
intermed_table1 <-gdata::combine(gender, Partner, Dependents, PhoneService, MultipleLines)

ggplot(intermed_table1, aes(x = source, y = Count, fill = Value)) + geom_bar(stat = "identity", position = "fill") + scale_fill_manual(values=colors) + theme(panel.background = element_blank())+scale_x_discrete(guide = guide_axis(n.dodge=2))+ xlab("")+ylab("Proportion")

# combine them in a second table and build the associated chart
intermed_table2 <-gdata::combine(InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies)

ggplot(intermed_table2, aes(x = source, y = Count, fill = Value)) + geom_bar(stat = "identity", position = "fill") + scale_fill_manual(values=colors) + theme(panel.background = element_blank())+scale_x_discrete(guide = guide_axis(n.dodge=2))+ xlab("")+ylab("Proportion")

# combine them in a last table and build the associated chart
intermed_table3 <-gdata::combine(Contract, PaperlessBilling, PaymentMethod)

ggplot(intermed_table3, aes(x = source, y = Count, fill = Value)) + geom_bar(stat = "identity", position = "fill") + scale_fill_manual(values=colors) + theme(panel.background = element_blank())+scale_x_discrete(guide = guide_axis(n.dodge=2))+ xlab("")+ylab("Proportion")

# correlation between the six additional services to Internet (Online security, Online Backup, Device Protection, Tech support, Streaming TV, Streaming movies) 
internet<-na.omit(data_original[,c(10:15,20)])
corrplot(cor(data.matrix(internet)), method="color",col=colorRampPalette(c(colors[5],"white",colors[2]))(200) )


## 2.4 Datasets preparation 

# deal with NAs
length(which(is.na(data_original))) 
data_clean<-na.omit(data_original)

# remove. customerID
data_clean<-data_clean[,-1]

# create a partition
set.seed(2, sample.kind = "Rounding") 
test_index <- createDataPartition(data_clean$Churn, times = 1, p = 0.20, list = FALSE)
test_set <- data_clean[test_index, ]
train_set <- data_clean[-test_index, ]

# convert to factor
train_set <- as.data.frame(unclass(train_set), stringsAsFactors = TRUE)
test_set <- as.data.frame(unclass(test_set), stringsAsFactors = TRUE)


## 2.5 Modeling

### 2.5.2 Building algorithms

#### 2.5.2.1 Logistic regression (glm)

# configurate the control parameters
fit_control_glm <- trainControl(method = "repeatedcv", number = 10, repeats = 50)

# train the model
fit_glm <- train(Churn ~ ., data = train_set, method = "glm", family = "binomial", trControl = fit_control_glm)

# build predictions and evaluate them 
predictions_glm <- predict(fit_glm, newdata = test_set)
cm_glm<-confusionMatrix(data = predictions_glm, reference = test_set$Churn, positive = "Yes")

# print the confusion matrix
cm_glm

# build a summary table of the results and print it
algo_results <- bind_rows(data_frame("Model #"="1", "Algorithm details"="Logistic regression (glm)", Accuracy = cm_glm$overall["Accuracy"], "Balanced Accuracy"= cm_glm[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()

#### 2.5.2.2 Nearest neighbors (knn)

# knn k optimization function
accuracy_function_knn <- function(x){
  fit_knn <- fit_knn <- knn3(Churn ~ ., data = train_set, k=x)
  predictions_knn <- predict(fit_knn, newdata = test_set, type = "class")
  confusionMatrix(data = predictions_knn, reference = test_set$Churn)$overall["Accuracy"]
}

# finding the k that optimizes the accuracy
knn_ks <- seq(1, 50, 1)
knn_accuracies<-sapply(knn_ks, accuracy_function_knn)
knn_kfinal<- knn_ks[which.max(knn_accuracies)]

# train the model
fit_knn <- knn3(Churn ~ ., data = train_set, k= knn_kfinal)

# build predictions and evaluate them 
predictions_knn <- predict(fit_knn, newdata = test_set, type = "class")
cm_knn<-confusionMatrix(data = predictions_knn, reference = test_set$Churn, positive = "Yes")

# print the confusion matrix
cm_knn

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="2", "Algorithm details"="Nearest neighbors (knn)", Accuracy = cm_knn$overall["Accuracy"], "Balanced Accuracy"= cm_knn[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()

#### 2.5.2.3 Decision tree (rpart)

# train the model with optimization embedded in the code
fit_rpart <- train(Churn ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = train_set)

# build predictions and evaluate them 
predictions_rpart <- predict(fit_rpart, newdata = test_set)
cm_rpart<-confusionMatrix(data = predictions_rpart, reference = test_set$Churn, positive ="Yes")

# visualise the tree
rpart.plot(fit_rpart$finalModel)

# print the confusion matrix 
cm_rpart

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="3", "Algorithm details"="Decision tree (rpart)", Accuracy = cm_rpart$overall["Accuracy"], "Balanced Accuracy"= cm_rpart[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()

#### 2.5.2.4 Random forest (rf)

# train the model with optimization embedded in the code 
fit_rf <-  randomForest(y=train_set$Churn, x=train_set[,-20], prox=TRUE)

# build predictions and evaluate them 
predictions_rf <- predict(fit_rf , newdata = test_set)
cm_rf<-confusionMatrix(data = predictions_rf, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix
cm_rf

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="4", "Algorithm details"="Random forest (rf)", Accuracy = cm_rf$overall["Accuracy"], "Balanced Accuracy"= cm_rf[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.5 Gradient boosting (gbm)

# train the model 
fit_gbm<-train(Churn~.,data=train_set, method ='gbm')

# build predictions and evaluate them 
predictions_gbm<-predict(fit_gbm, newdata = test_set)
cm_gbm<-confusionMatrix(data = predictions_gbm, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix 
cm_gbm

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="5", "Algorithm details"="Gradient boosting (gbm)", Accuracy = cm_gbm$overall["Accuracy"], "Balanced Accuracy"= cm_gbm[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.6 Multilayer perceptron (mlp)

# train the model 
fit_mlp<-train(Churn~.,data=train_set, method ='mlp')

# build predictions and evaluate them 
predictions_mlp<-predict(fit_mlp, newdata = test_set)
cm_mlp<-confusionMatrix(data = predictions_mlp, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix
cm_mlp

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="6", "Algorithm details"="Multilayer perceptron (mlp)", Accuracy = cm_mlp$overall["Accuracy"], "Balanced Accuracy"= cm_mlp[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.7 Naïve bayes (nb)

# train the model 
fit_nb<-train(Churn~.,data=train_set, method ='naive_bayes')

# build predictions and evaluate them 
predictions_nb<-predict(fit_nb, newdata = test_set)
cm_nb<-confusionMatrix(data = predictions_nb, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix
cm_nb

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="7", "Algorithm details"="Naïve bayes (nb)", Accuracy = cm_nb$overall["Accuracy"], "Balanced Accuracy"= cm_nb[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.8 Bayesian generalized linear model (bglm)

# train the model 
fit_bglm<-train(Churn~.,data=train_set, method ='bayesglm')

# build predictions and evaluate them 
predictions_bglm<-predict(fit_bglm, newdata = test_set)
cm_bglm<-confusionMatrix(data = predictions_bglm, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix 
cm_bglm

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="8", "Algorithm details"="Bayesian GLM (bglm)", Accuracy = cm_bglm$overall["Accuracy"], "Balanced Accuracy"= cm_bglm[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.9 Neural network (nn)

# train the model 
fit_nn<-train(Churn~.,data=train_set, method ='pcaNNet')

# build predictions and evaluate them 
predictions_nn<-predict(fit_nn, newdata = test_set)
cm_nn<-confusionMatrix(data = predictions_nn, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix 
cm_nn

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="9", "Algorithm details"="Neural network (nn)", Accuracy = cm_nn$overall["Accuracy"], "Balanced Accuracy"= cm_nn[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


#### 2.5.2.10 Support vector machine (svm)

# train the model 
fit_svm<-train(Churn~.,data=train_set, method ='svmLinearWeights')

# build predictions and evaluate them 
predictions_svm<-predict(fit_svm, newdata = test_set)
cm_svm<-confusionMatrix(data = predictions_svm, reference = test_set$Churn, positive ="Yes")

# print the confusion matrix 
cm_svm

# build a summary table of the results and print it
algo_results <- bind_rows(algo_results,data_frame("Model #"="10", "Algorithm details"="Support vector machine (svm)", Accuracy = cm_svm$overall["Accuracy"], "Balanced Accuracy"= cm_svm[["byClass"]][["Balanced Accuracy"]]))
algo_results %>% knitr::kable()


### 2.5.3 Build the ensemble algorithm

# show the 7 best models based on accuracy 
knitr::kable(head((algo_results[order(-algo_results$Accuracy),]),n=7))

# build a table with the predictions of those 7 models 

# building the overall table with the 7 predictions
ensemble <-data.frame(predictions_glm, predictions_knn, predictions_rpart, predictions_rf, predictions_gbm, predictions_bglm, predictions_nn)

# study correlation
ensemble_matrix<-data.matrix(ensemble)
corrplot(cor(ensemble_matrix), method="color",col=colorRampPalette(c(colors[5],"white",colors[2]))(200) )

# build the esemble models

#building the ensembles
ensemble1<- as.factor (ifelse(rowSums(ensemble_matrix[,c(1:3)]-1)>1,"Yes","No"))
ensemble2<-as.factor(ifelse(rowSums(ensemble_matrix[,c(1:5)]-1)>2,"Yes","No"))
ensemble3<-as.factor(ifelse(rowSums(ensemble_matrix[,c(1:7)]-1)>3,"Yes","No"))

# computing the confusion matrices
cm_e1<- confusionMatrix(data = ensemble1, reference = test_set$Churn, positive ="Yes")
cm_e2<- confusionMatrix(data = ensemble2, reference = test_set$Churn, positive ="Yes")
cm_e3<- confusionMatrix(data = ensemble3, reference = test_set$Churn, positive ="Yes")

# adding the results to the result table
algo_results <- bind_rows(algo_results,data_frame("Model #"="E1", "Algorithm details"="Ensemble 1", Accuracy = cm_e1$overall["Accuracy"], "Balanced Accuracy"= cm_e1[["byClass"]][["Balanced Accuracy"]]))
algo_results <- bind_rows(algo_results,data_frame("Model #"="E2", "Algorithm details"="Ensemble 2", Accuracy = cm_e2$overall["Accuracy"], "Balanced Accuracy"= cm_e2[["byClass"]][["Balanced Accuracy"]]))
algo_results <- bind_rows(algo_results,data_frame("Model #"="E3", "Algorithm details"="Ensemble 3", Accuracy = cm_e3$overall["Accuracy"], "Balanced Accuracy"= cm_e3[["byClass"]][["Balanced Accuracy"]]))

algo_results %>% knitr::kable()


# 3. Results

## 3.1 Best model

# Accuracy 
round(cm_e1$overall["Accuracy"],4)*100

# Balanced accuracy
round(cm_e1[["byClass"]][["Balanced Accuracy"]],4)*100
  

## 3.2 Feature importance

# visualize the importance

# build the decision tree from rpart package in order to use the importance functionality of the package
fit_rpart2 <- rpart(Churn ~ ., data = train_set) 
predictions_rpart2 <- predict(fit_rpart2, test_set,type="class")

# building the tables and the chart to visualize importance of features of both models
rpart_importance <- as.data.frame(fit_rpart2$variable.importance)
rpart_importance <- cbind(Feature = rownames(rpart_importance), rpart_importance)

rf_importance <- as.data.frame(randomForest::importance(fit_rf))
rf_importance <- cbind(Feature = rownames(rf_importance), rf_importance)

colnames(rpart_importance)<-c("Feature","Importance")
colnames(rf_importance)<-c("Feature","Importance")

rownames(rpart_importance)<-NULL
rownames(rf_importance)<-NULL

importance <- gdata::combine(rf_importance, rpart_importance)
colnames(importance)<- c("Feature","Importance","Model")

# visualise the importance of the features
importance %>% mutate(Feature = fct_reorder(Feature, Importance)) %>% ggplot(aes(fill=Model, y=Importance, x=Feature)) + geom_bar(position="dodge", stat="identity") + coord_flip()+scale_fill_manual(values=c(colors[1],colors[3]) )+ theme(panel.background = element_blank())
