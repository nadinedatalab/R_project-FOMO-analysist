# ==============================================================================
# FILE 1: HUỐN LUYỆN VÀ ĐÁNH GIÁ MÔ HÌNH HỌC MÁY RANDOM FOREST (NHẬT MINH)
# ==============================================================================

# 1. Cấu hình thư mục gốc đồ án và nạp các thư viện thực chiến
SET_PATH <- "D:/ki_2_nam_2/Dot 2/R/Do_An"
setwd(SET_PATH)

library(data.table)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)

# 2. Đọc tệp dữ liệu thực tế (Train và Test)
train_df <- fread("cleaned_research_train.csv", stringsAsFactors = TRUE)
test_df  <- fread("cleaned_research_test.csv", stringsAsFactors = TRUE)

# 3. Chuẩn hóa biến mục tiêu nhị phân sang định dạng Factor
train_df$purchased <- factor(train_df$purchased, levels = c("No", "Yes"))
test_df$purchased  <- factor(test_df$purchased, levels = c("No", "Yes"))

# 4. Lọc danh sách đặc trưng đầu vào ánh xạ từ phễu nhận thức S-O-R
features_list <- c("age", "estimated_salary", "daily_time_spent", "scroll_rate_ppm",
                   "scarcity_exposure", "fomo_index", "Flow_Experience",
                   "Extraversion_Score", "Neuroticism_Score", "Conscientiousness_Score",
                   "Openness_Score", "Agreeableness_Score", "purchased")

train_set <- train_df %>% select(all_of(features_list))
test_set  <- test_df %>% select(all_of(features_list))

# 5. Cấu hình bộ điều khiển kiểm tra chéo 10 vùng (10-fold Cross-Validation)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, # Tính toán chỉ số diện tích ROC
  savePredictions = "final"
)

# 6. Huấn luyện thuật toán Rừng ngẫu nhiên và tối ưu siêu tham số mtry
cat("-> Hệ thống đang chạy Random Forest trên 10-fold CV...\n")
set.seed(2026)
model_rf <- train(
  purchased ~ .,
  data = train_set,
  method = "rf",
  trControl = train_control,
  metric = "ROC",
  tuneLength = 3, # Tự động tìm kiếm mtry tối ưu
  importance = TRUE
)

# 7. Dự báo kết quả thực nghiệm trên tập dữ liệu Test Set độc lập
rf_preds_class <- predict(model_rf, newdata = test_set)
rf_preds_prob  <- predict(model_rf, newdata = test_set, type = "prob")

# 8. Trích xuất ma trận nhầm lẫn Confusion Matrix của Random Forest
conf_mat_rf <- confusionMatrix(rf_preds_class, test_set$purchased, positive = "Yes")
print(conf_mat_rf)

# 9. Đánh giá và trực quan hóa độ quan trọng biến (Feature Importance)
feature_importance <- varImp(model_rf)
print(feature_importance)
plot(feature_importance, main = "Độ Quan Trọng Đặc Trưng Hệ Thống - Mô Hình Random Forest")

# 10. Lưu lại kết quả dự báo xác suất của RF sang file tạm để phục vụ file 2 đối sánh
save(rf_preds_prob, conf_mat_rf, file = "rf_output_data.RData")
cat("-> Hoàn thành giai đoạn huấn luyện RF. Đã lưu kết quả dự báo.\n")