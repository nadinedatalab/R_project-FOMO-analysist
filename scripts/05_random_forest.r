

SET_PATH <- "D:/ki_2_nam_2/Dot 2/R/Do_An"
setwd(SET_PATH)

library(data.table)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)

train_df <- fread("cleaned_research_train.csv", stringsAsFactors = TRUE)
test_df  <- fread("cleaned_research_test.csv", stringsAsFactors = TRUE)

train_df$purchased <- factor(train_df$purchased, levels = c("No", "Yes"))
test_df$purchased  <- factor(test_df$purchased, levels = c("No", "Yes"))

features_list <- c("age", "estimated_salary", "daily_time_spent", "scroll_rate_ppm",
                   "scarcity_exposure", "fomo_index", "Flow_Experience",
                   "Extraversion_Score", "Neuroticism_Score", "Conscientiousness_Score",
                   "Openness_Score", "Agreeableness_Score", "purchased")

train_set <- train_df %>% select(all_of(features_list))
test_set  <- test_df %>% select(all_of(features_list))

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  savePredictions = "final"
)

cat("-> Hệ thống đang chạy Random Forest trên 10-fold CV...\n")
set.seed(2026)
model_rf <- train(
  purchased ~ .,
  data = train_set,
  method = "rf",
  trControl = train_control,
  metric = "ROC",
  tuneLength = 3, 
  importance = TRUE
)

print(model_rf)

rf_preds_class <- predict(model_rf, newdata = test_set)
rf_preds_prob  <- predict(model_rf, newdata = test_set, type = "prob")

conf_mat_rf <- confusionMatrix(rf_preds_class, test_set$purchased, positive = "Yes")
print(conf_mat_rf)

roc_rf <- roc(test_set$purchased, rf_preds_prob$Yes, levels = c("No", "Yes"))
cat("AUC-ROC của mô hình Random Forest trên Test Set:", auc(roc_rf), "\n")

feature_importance <- varImp(model_rf)
print(feature_importance)
plot(feature_importance, main = "Độ Quan Trọng Đặc Trưng Hệ Thống - Mô Hình Random Forest")

save(rf_preds_prob, conf_mat_rf, file = "rf_output_data.RData")
cat("-> Hoàn thành giai đoạn huấn luyện RF. Đã lưu kết quả dự báo.\n")
