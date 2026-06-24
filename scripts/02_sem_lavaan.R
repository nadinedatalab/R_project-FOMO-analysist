# Mục tiêu: Kiểm định cơ chế nhân quả S-O-R
# X = daily_time_spent, M = fomo_index, Y = purchase_prob

library(lavaan)
train <- read.csv("data/cleaned_research_train.csv")
test  <- read.csv("data/cleaned_research_test.csv")

nrow(train)   # 8000
names(train)  # 22 cột
summary(train[, c("daily_time_spent", "fomo_index", 
                  "Flow_Experience", "purchase_prob")])


# 2. MÔ HÌNH 1: SIMPLE MEDIATION (X -> M -> Y) 
model_sem1 <- '
  # Path a: X tác động lên M
  fomo_index ~ a * daily_time_spent
  
  # Path b và c prime: M và X tác động lên Y
  purchase_prob ~ b * fomo_index + cprime * daily_time_spent
  
  # Tính các hiệu ứng
  indirect := a * b
  direct   := cprime
  total    := a * b + cprime
  prop_med := (a * b) / (a * b + cprime)
'

# Ước lượng mô hình
fit_sem1 <- sem(model_sem1, data = train)
summary(fit_sem1, fit.measures = TRUE, rsquare = TRUE)

# Bootstrap 1000 lần để có CI chính xác
fit_boot <- sem(model_sem1, data = train,
                se = "bootstrap", bootstrap = 1000)

# Xuất bảng kết quả Bootstrap
parameterEstimates(fit_boot,
                   boot.ci.type = "bca.simple",
                   level = 0.95) |>
  subset(label %in% c("indirect", "direct", "total", "prop_med"))


# 3. MÔ HÌNH 2: SERIAL MEDIATION (X -> M1 -> M2 -> Y)
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