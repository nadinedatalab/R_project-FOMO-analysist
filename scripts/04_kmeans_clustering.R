# Mục tiêu: Phân khúc khách hàng theo hành vi số
# Unsupervised Learning — Clustering

library(cluster)
library(factoextra)
library(ggplot2)

train <- read.csv("data/cleaned_research_train.csv")

# 2. CHỌN BIẾN VÀ CHUẨN HÓA
# 5 biến được chọn dựa trên kết quả Path Analysis và Logistic Regression
vars_cluster <- train[, c("daily_time_spent", "fomo_index", 
                          "scarcity_exposure", "Conscientiousness_Score",
                          "purchase_prob")]

# Chuẩn hóa z-score (bắt buộc vì K-Means dùng khoảng cách Euclidean)
vars_scaled <- scale(vars_cluster)


#  3. TÌM K TỐI ƯU BẰNG ELBOW METHOD
elbow_plot <- fviz_nbclust(vars_scaled, kmeans, 
                           method = "wss", k.max = 10) +
  labs(title = "Elbow Method - Chọn số cụm K")

print(elbow_plot)

ggsave("output/elbow_method.png", 
       plot = elbow_plot, width = 6, height = 4, dpi = 300)

#  4. CHẠY K-MEANS VỚI K=4 
set.seed(123)  # Đảm bảo kết quả tái lập được

km_result <- kmeans(vars_scaled, centers = 4, nstart = 25)

# Số người mỗi cụm
table(km_result$cluster)
train$cluster <- as.factor(km_result$cluster)

cluster_profile <- aggregate(
  train[, c("daily_time_spent", "fomo_index", 
            "scarcity_exposure", "Conscientiousness_Score",
            "purchase_prob")], 
  by  = list(cluster = train$cluster), 
  FUN = mean
)
print(round(cluster_profile, 3))

# Đặt tên phân khúc dựa trên đặc điểm
# Cluster 2: FOMO Buyer     (daily_time cao, fomo cao, purchase_prob cao nhất)
# Cluster 1: Engaged Shopper (fomo khá cao, purchase_prob cao)
# Cluster 3: Casual Browser  (fomo thấp, purchase_prob trung bình)
# Cluster 4: Rational Saver  (Conscientiousness cao, purchase_prob thấp nhất)

# VẼ BIỂU ĐỒ PHÂN CỤM
cluster_plot <- fviz_cluster(
  km_result, 
  data         = vars_scaled,
  geom         = "point",
  ellipse.type = "convex",
  palette      = "jco",
  ggtheme      = theme_minimal()
) +
  labs(title = "Phân khúc khách hàng theo K-Means (K=4)")

print(cluster_plot)

# Lưu biểu đồ Cluster
ggsave("output/kmeans_cluster.png", 
       plot = cluster_plot, width = 7, height = 5, dpi = 300)