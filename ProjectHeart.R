heart <- read.csv(file = "C:/Users/bsjun/Downloads/R working directory/heart.csv")

install.packages("randomForest")
library(randomForest)
heart$Sex<-as.factor(heart$Sex)
heart$ChestPainType<-as.factor(heart$ChestPainType)
heart$RestingECG<-as.factor(heart$RestingECG)
heart$ExerciseAngina<-as.factor(heart$ExerciseAngina)
heart$ST_Slope<-as.factor(heart$ST_Slope)
heart$HeartDisease<-as.factor(heart$HeartDisease)

#10-fold 분류
install.packages("caret")
library(caret)
k_fold_result <- createFolds(heart$HeartDisease, k = 10)
prop.true <- c()

#randomforest로 가장 예측력 좋은 fold 찾기
for(i in 1:10){
  rf.heart <- randomForest(HeartDisease ~ ., data = heart[-k_fold_result[[i]],], mtry = 5, importance = TRUE)
  yhat.rf <- predict(rf.heart, newdata = heart[k_fold_result[[i]],])
  heart.test <- heart[k_fold_result[[i]],"HeartDisease"]
  
  table(yhat.rf, heart.test)
  prop.true[i] <- mean(yhat.rf == heart.test)
}
prop.true

#위에서 구한 fold 중 예측력 좋은 fold를 test로 모델링
train <- heart[k_fold_result[[which.max(prop.true)]],]
test <- heart[-k_fold_result[[which.max(prop.true)]],]

rf.heart <- randomForest(HeartDisease~., data = heart[-k_fold_result[[which.max(prop.true)]],], mtry = 5, importance = TRUE)
yhat.rf <- predict(rf.heart, newdata = heart[k_fold_result[[which.max(prop.true)]],])
heart.test <- heart[k_fold_result[[which.max(prop.true)]],"HeartDisease"]


#AUC 구하기
library(pROC)
pred_validation_num <- as.numeric(yhat.rf)
result_validation <- roc(heart.test, pred_validation_num)

plot.roc(result_validation,legacy.axes = TRUE)
result_validation$auc


library(caret)
set.seed(1)
k_fold_result<- createFolds(heart$HeartDisease, k=10)
cv.error<-c()
auc<-c()

library(e1071)

for(i in 1:10){
  sv.heart <- svm(HeartDisease~., data=heart[-k_fold_result[[i]],], type="C-classification")
  yhat.sv <- predict(sv.heart, newdata=heart[k_fold_result[[i]],])
  heart.test <- heart[k_fold_result[[i]],"HeartDisease"]
  cv.error[i] <- mean(yhat.sv!=heart.test)
  pred_validation_num <- as.numeric(yhat.sv)
  result_validation <- roc(heart.test, pred_validation_num)
  auc[i] <- result_validation$auc
}

cv.error
mean(cv.error)
auc
mean(auc)
plot(auc, pch=16, lwd=2, col="darkgrey")
lines(auc)
plot(cv.error)