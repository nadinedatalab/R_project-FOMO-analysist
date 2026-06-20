library(caret)
library(pROC)

# Load lại data (để file này chạy độc lập, không phụ thuộc file khác)
train <- read.csv("data/cleaned_research_train.csv")
test  <- read.csv("data/cleaned_research_test.csv")

# Chuyển purchased thành factor (caret yêu cầu)
train$purchased <- factor(train$purchased, levels = c("No", "Yes"))
test$purchased  <- factor(test$purchased, levels = c("No", "Yes"))

# Xây dựng mô hình Logistic Regression
model_logit <- glm(purchased ~ daily_time_spent + scarcity_exposure + 
                     fomo_index + Flow_Experience + 
                     Neuroticism_Score + Conscientiousness_Score +
                     Openness_Score + Agreeableness_Score +
                     age + estimated_salary + platform + gender,
                   data = train, family = binomial(link = "logit"))

# Xem kết quả
summary(model_logit)

# Tính Odds Ratio và khoảng tin cậy 95%
odds_ratios <- exp(cbind(OR = coef(model_logit), confint(model_logit)))
print(round(odds_ratios, 3))

# Dự báo xác suất trên tập test
pred_prob <- predict(model_logit, newdata = test, type = "response")

# Phân loại Yes/No với ngưỡng 0.5
pred_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))

# Confusion Matrix
confusionMatrix(pred_class, test$purchased)

# ROC-AUC
roc_logit <- roc(test$purchased, pred_prob)
auc(roc_logit)
confusionMatrix(pred_class, test$purchased, positive = "Yes")

library(pROC)
library(ggplot2)

ggroc(roc_logit, colour = "#378ADD", linewidth = 1) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey60") +
  labs(title = "ROC Curve - Logistic Regression",
       subtitle = paste("AUC =", round(auc(roc_logit), 3)),
       x = "Specificity", y = "Sensitivity") +
  theme_minimal()