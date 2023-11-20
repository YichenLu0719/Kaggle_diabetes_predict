library(ggplot2)
library(corrplot)
library(car)
library("ROCR")
diabetes<-read.csv("C:\\Users\\dl\\Desktop\\yc_data\\糖尿病預測\\diabetes_data.csv")
#記錄此csv檔有甚麼資料以及資料的描述
whatIhave <- data.frame("變數"=names(diabetes))
descrimination<-c("13個等級的年齡組 1=18~24、2=25~29、3=30~34、4=35~39、5=40~44、6=45~49、7=50~54、8=55~59、9=60~64、13=80歲或以上",
        "患者性別(1:男性,2:女性)",
        "0=沒有高膽固醇、1=高膽固醇",
        "0=5年內未檢查膽固醇、1 = 5年內檢查過膽固醇",
        "體重指數",
        "您一生中至少吸過 100 支香煙嗎？[注：5包=100支香煙] 0=否、1=是",
        "冠心病(CHD)或心肌梗死(MI) 0=否、1=是",
        "過去30天的體力活動-不包括工作 0=否、1=是",
        "每天吃水果 1 次或更多次 0=否、1=是",
        "每天吃蔬菜 1 次或更多次 0=否、1=是",
        "酒精（成年男性每週 >=14 杯，成年女性每週 >=7 杯）0=否、1=是",
        "你會說一般來說你的健康狀況是：等級:1-5 1=極好、2=非常好、3=好、4=一般、5=差",
        "心理健康狀況不佳天數1-30天",
        "過去30天內身體生病或受傷的天數1-30",
        "你走路或爬樓梯有嚴重困難嗎？ 0=否、1=是",
        "是否中風過。0=否、1=是",
        "高血壓:0=不高， 1=高 ",
        "0=沒有糖尿病、1 =有糖尿病")
whatIhave <-cbind(whatIhave,"描述"=descrimination)

#計算各個變數的遺漏值(python:df.isnull().sum())
isnull_data<-sapply(diabetes, function(x)sum(is.na(x)))
isnull_df<-data.frame("null_count"=isnull_data)

#評估每一列的唯一值
unique_df<- data.frame("變數"=names(diabetes))
unique_df2<-data.frame()
for (i in 1:18) {
  unique_df2<-rbind(unique_df2,length(unique(diabetes[,i])))
  }
unique_df<-data.frame("變數"=unique_df$變數,"唯一值個數"=unique_df2$X13L)
###############抓出全部名稱跑迴圈###############
names(diabetes)
for (i in 1:length(diabetes)){
  barplot(table(diabetes[,i]),xlab = names(diabetes)[i],ylab = "次數",main=names(diabetes)[i])
}

###########觀察相關性#############
cor_df<-data.frame(cor(diabetes))
sort(cor_df$Diabetes)
corrplot(cor(diabetes),method = 'color',tl.cex = 0.6,tl.srt = 45) 

############把那些可能對模型貢獻度不足的數據刪除#############
diabetes<-diabetes[,-c(2,4,6,8,9,10,11,16)]
################切割為訓練與測試(8:2)##################
index <-sort(sample(nrow(diabetes), nrow(diabetes)*.8))
train <- diabetes[index,]#訓練
test <-diabetes[-index,]#測試
X_train<-train[,-c(10)]
y_train<-train[,c(10)]
X_test<-test[,-c(10)]
y_test<-test[,c(10)]
###################正規化#################
X_train$Age <- (X_train$Age - min(X_train$Age))/(max(X_train$Age) - min(X_train$Age))
X_train$HighChol <- (X_train$HighChol - min(X_train$HighChol))/(max(X_train$HighChol) - min(X_train$HighChol))
X_train$BMI <- (X_train$BMI - min(X_train$BMI))/(max(X_train$BMI) - min(X_train$BMI))
X_train$HeartDiseaseorAttack <- (X_train$HeartDiseaseorAttack - min(X_train$HeartDiseaseorAttack))/(max(X_train$HeartDiseaseorAttack) - min(X_train$HeartDiseaseorAttack))
X_train$GenHlth <- (X_train$GenHlth - min(X_train$GenHlth))/(max(X_train$GenHlth) - min(X_train$GenHlth))
X_train$MentHlth <- (X_train$MentHlth - min(X_train$MentHlth))/(max(X_train$MentHlth) - min(X_train$MentHlth))
X_train$PhysHlth <- (X_train$PhysHlth - min(X_train$PhysHlth))/(max(X_train$PhysHlth) - min(X_train$PhysHlth))
X_train$DiffWalk <- (X_train$DiffWalk - min(X_train$DiffWalk))/(max(X_train$DiffWalk) - min(X_train$DiffWalk))
X_train$HighBP <- (X_train$HighBP - min(X_train$HighBP))/(max(X_train$HighBP) - min(X_train$HighBP))

X_test$Age <- (X_test$Age - min(X_test$Age))/(max(X_test$Age) - min(X_test$Age))
X_test$HighChol <- (X_test$HighChol - min(X_test$HighChol))/(max(X_test$HighChol) - min(X_test$HighChol))
X_test$BMI <- (X_test$BMI - min(X_test$BMI))/(max(X_test$BMI) - min(X_test$BMI))
X_test$HeartDiseaseorAttack <- (X_test$HeartDiseaseorAttack - min(X_test$HeartDiseaseorAttack))/(max(test$HeartDiseaseorAttack) - min(X_train$HeartDiseaseorAttack))
X_test$GenHlth <- (X_test$GenHlth - min(X_test$GenHlth))/(max(X_test$GenHlth) - min(X_test$GenHlth))
X_test$MentHlth <- (X_test$MentHlth - min(X_test$MentHlth))/(max(X_test$MentHlth) - min(X_test$MentHlth))
X_test$PhysHlth <- (X_test$PhysHlth - min(X_test$PhysHlth))/(max(X_test$PhysHlth) - min(X_test$PhysHlth))
X_test$DiffWalk <- (X_test$DiffWalk - min(X_test$DiffWalk))/(max(X_test$DiffWalk) - min(X_test$DiffWalk))
X_test$HighBP <- (X_test$HighBP - min(X_test$HighBP))/(max(X_test$HighBP) - min(X_test$HighBP))

############建立羅吉斯回歸模型############
stroke_glm<-glm(y_train~Age+HighChol+BMI+HeartDiseaseorAttack+GenHlth+MentHlth+PhysHlth+DiffWalk+HighBP,data=X_train)
summary(stroke_glm)
############診斷共線性##############
vif(stroke_glm)
###########預測與混淆矩陣###########
y_pred<-predict(stroke_glm,newdata=X_test)
result_Approved <- ifelse(y_pred > 0.6, 1, 0)
cm <- table(y_test, result_Approved, dnn = c("實際", "預測"))
cm
#計算糖尿病正確率
cm[4] / sum(cm[, 2])

#計算非糖尿病正確率
cm[1] / sum(cm[, 1])
#整體準確率(對角線元素總和/所有觀察值總和)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy
#畫ROC曲線
pred <- prediction(y_pred, y_test)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#計算AUC
auc <- performance(pred, "auc")

#畫圖
plot(perf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))
#######測試集#########
fun<-data.frame(Age=(1- min(diabetes$Age))/(max(diabetes$Age) - min(diabetes$Age)),
                HighChol=2,
                BMI=(22- min(diabetes$BMI))/(max(diabetes$BMI) - min(diabetes$BMI)),
                HeartDiseaseorAttack=0,
                GenHlth=(1- min(diabetes$GenHlth))/(max(diabetes$GenHlth) - min(diabetes$GenHlth)),
                MentHlth=(2- min(diabetes$MentHlth))/(max(diabetes$MentHlth) - min(diabetes$MentHlth)),
                PhysHlth=(2- min(diabetes$PhysHlth))/(max(diabetes$PhysHlth) - min(diabetes$PhysHlth)),
                DiffWalk=0,
                HighBP=0)
y_pred_fun<-predict(stroke_glm,newdata=fun)
y_pred_fun
table(diabetes$GenHlth)
y_pred
