data<-read.csv("D:/Data KMMI.csv", header = TRUE, sep = ",")
data$BMI[is.na(data$BMI)]<-median(data$BMI,na.rm=TRUE)
# Libraries
library(ggplot2)
library(dplyr)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
data%>%
  ggplot( aes(x=Stroke, fill=Stroke)) + 
  geom_bar()+
  scale_fill_manual(values = c("#CB7A09","#0D8295")) +
  theme(legend.position="none")
prop.table(table(data$Stroke))
#Create Data Partition
set.seed(1234)
index <- createDataPartition(data$Stroke, p=0.80, list=FALSE)
# select 80% of the data for Training
train <- data[index,]
dim(train)
# use the remaining 80% of data to testing the models
test <- data[-index,]
dim(test)


#Over Sampling (mengambil kelas mayoritas)
over=ovun.sample(Stroke~., data=train, method="over", N=408)$data
table(over$Stroke)
#Pemodelan dengan over sampling
set.seed(120) #setting seed
classifier_cl=naiveBayes(Stroke ~ .,data=over)
y_pred=predict(classifier_cl, test)
cm<-table(y_pred,test$Stroke)
confusionMatrix(cm)

#Metode under sampling (mengambil kelas minoritas)
under=ovun.sample(Stroke~., data=train, method="under", N=72)$data
table(under$Stroke)
set.seed(120)
classifier_cl=naiveBayes(Stroke ~ .,data=over)
y_pred2=predict(classifier_cl, test)
cm<-table(y_pred2,test$Stroke)
confusionMatrix(cm)

#Both
both=ovun.sample(Stroke~., data=train, method="both",
                 p=0.5,
                 seed=222,
                 N=240)$data
table(both$Stroke)
classifier_cl=naiveBayes(Stroke~., data=both)
y_predict=predict(classifier_cl, test)
cm<-table(y_predict,test$Stroke)
confusionMatrix(cm)

# ROSE FUNCTION
rose = ROSE(as.factor(Stroke)~., data = train, N = 500, seed=111)$data
table(rose$Stroke)

#SMOTE (Synthetic Minority Sampling Technique)
library(performanceEstimation)
smote=smote(as.factor(Stroke)~., train, perc.over=2, k=5, perc.under=2)
table(smote$Stroke)
classifier_s <- naiveBayes(Stroke ~ ., data = smote)
y_pred3=predict(classifier_cl, test)
cm<-table(y_pred3,test$Stroke)
confusionMatrix(cm)

#EVALUATION METRICS
#Create Data Partition
library(caret)
library(dplyr)
set.seed(1234)
index <- createDataPartition(data$Stroke, p=0.80, list=FALSE)
# select 80% of the data for Training
train <- data[index,]
# use the remaining 80% of data to testing the models
test <- data[-index,]
library(caTools)
library(e1071)
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Stroke ~ ., data = train)
# Predicting on training data'
y_pred <- predict(classifier_cl, newdata = test)
# Confusion Matrix
cm <- table(test$Stroke, y_pred)
# Model Evaluation
confusionMatrix(cm)

#Calculate recall
cm<-table(y_pred,test$Stroke)
CM=confusionMatrix(cm, mode="prec_recall")
CM

# Calculate Area Under Curve (AUC)
library(cvAUC)
auc <- AUC(as.numeric(y_pred),as.numeric(test$Stroke))
auc
#Receiver Operating Characterictic (ROC)
roc.curve(test$Stroke,y_pred)

library(precrec)
precrec_obj=evalmod(scores=as.numeric(y_pred), labels=as.numeric(test$Stroke))
autoplot(precrec_obj)
