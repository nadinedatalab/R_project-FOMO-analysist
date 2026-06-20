library(cluster)
library(factoextra)

train <- read.csv("data/cleaned_research_train.csv")

# Chọn biến và chuẩn hóa (QUAN TRỌNG - K-Means nhạy với scale)
vars_cluster <- train[, c("daily_time_spent", "fomo_index", 
                          "scarcity_exposure", "Conscientiousness_Score",
                          "purchase_prob")]

vars_scaled <- scale(vars_cluster)

# Bước 1: Tìm K tối ưu bằng Elbow method
fviz_nbclust(vars_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Elbow Method - Chọn số cụm K")

set.seed(123)  # đảm bảo kết quả tái lập được

km_result <- kmeans(vars_scaled, centers = 4, nstart = 25)

# Xem số người mỗi cụm
table(km_result$cluster)

# Gán cụm vào data gốc
train$cluster <- as.factor(km_result$cluster)

# Xem đặc điểm trung bình từng cụm (chưa chuẩn hóa - dễ đọc)
aggregate(train[, c("daily_time_spent", "fomo_index", 
                    "scarcity_exposure", "Conscientiousness_Score",
                    "purchase_prob")], 
          by = list(cluster = train$cluster), 
          FUN = mean)
fviz_cluster(km_result, data = vars_scaled,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  labs(title = "Phân khúc khách hàng theo K-Means (K=4)")