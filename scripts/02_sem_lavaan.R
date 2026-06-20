library(lavaan)

# Load data mới (22 cột)
train <- read.csv("data/cleaned_research_train.csv")
test  <- read.csv("data/cleaned_research_test.csv")

# Kiểm tra
nrow(train)
names(train)
summary(train[, c("daily_time_spent", "fomo_index", "Flow_Experience", "purchase_prob")])

model_sem2 <- '
  # X -> M1 (FOMO)
  fomo_index ~ a1 * daily_time_spent
  
  # M1 -> M2 (Flow), kiểm soát X
  Flow_Experience ~ a2 * fomo_index + d1 * daily_time_spent
  
  # M2 -> Y, kiểm soát M1 và X
  purchase_prob ~ b * Flow_Experience + b1 * fomo_index + cprime * daily_time_spent
  
  # Tác động gián tiếp nối tiếp (X -> M1 -> M2 -> Y)
  indirect_serial := a1 * a2 * b
  
  # Tổng tác động
  total2 := (a1 * a2 * b) + (a1 * b1) + cprime
  
  # Tỷ lệ trung gian nối tiếp
  prop_med_serial := indirect_serial / total2
'

fit_sem2 <- sem(model_sem2, data = train)
summary(fit_sem2, fit.measures = TRUE, rsquare = TRUE)