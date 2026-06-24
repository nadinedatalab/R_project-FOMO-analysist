# Mục tiêu: Dự báo hành vi mua hàng (Yes/No)
# Supervised Learning — Binary Classification


library(caret)
library(pROC)
library(ggplot2)

train <- read.csv("data/cleaned_research_train.csv")
test  <- read.csv("data/cleaned_research_test.csv")

# Chuyển purchased thành factor (bắt buộc với caret và glm)
train$purchased <- factor(train$purchased, levels = c("No", "Yes"))
test$purchased  <- factor(test$purchased,  levels = c("No", "Yes"))

#  2. XÂY DỰNG MÔ HÌNH 
model_logit <- glm(
  purchased ~ daily_time_spent + scarcity_exposure + 
    fomo_index + Flow_Experience + 
    Neuroticism_Score + Conscientiousness_Score +
    Openness_Score + Agreeableness_Score +
    age + estimated_salary + platform + gender,
  data   = train,
  family = binomial(link = "logit")
)

summary(model_logit)

# 3. ODDS RATIO 
odds_ratios <- exp(cbind(OR = coef(model_logit), confint(model_logit)))
print(round(odds_ratios, 3))


#  4. DỰ BÁO TRÊN TẬP TEST 
# Tính xác suất mua hàng
pred_prob <- predict(model_logit, newdata = test, type = "response")

# Phân loại Yes/No với ngưỡng 0.5
pred_class <- factor(
  ifelse(pred_prob > 0.5, "Yes", "No"), 
  levels = c("No", "Yes")
)

# 5. ĐÁNH GIÁ MÔ HÌNH 
# Confusion Matrix (Positive = "Yes" = có mua)
confusionMatrix(pred_class, test$purchased, positive = "Yes")

# ROC-AUC
roc_logit <- roc(test$purchased, pred_prob)
auc(roc_logit)  # Kết quả: 0.896

# 6. VẼ ROC CURVE 
roc_plot <- ggroc(roc_logit, colour = "#378ADD", linewidth = 1) +
  geom_abline(intercept = 1, slope = 1, 
              linetype = "dashed", color = "grey60") +
  labs(
    title    = "ROC Curve - Logistic Regression",
    subtitle = paste("AUC =", round(auc(roc_logit), 3)),
    x        = "Specificity",
    y        = "Sensitivity"
  ) +
  theme_minimal()

print(roc_plot)
ggsave("output/roc_curve_logistic.png", 
       plot = roc_plot, width = 6, height = 5, dpi = 300)