# ============================================
# EDA: TỶ LỆ PURCHASED (YES/NO)
# ============================================

library(ggplot2)
library(readr)
library(dplyr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Tính tỷ lệ
purchase_rate <- train %>%
  group_by(purchased) %>%
  summarise(count = n()) %>%
  mutate(rate = count / sum(count) * 100)

# Màu
purchase_colors <- c(
  "Yes" = "#B5EAD7",
  "No"  = "#FFB7B2"
)

# Vẽ biểu đồ
ggplot(purchase_rate, aes(x = purchased, y = rate, fill = purchased)) +
  geom_col(width = 0.5, alpha = 0.95) +
  geom_text(aes(label = paste0(round(rate, 1), "%")),
            vjust = -0.5, size = 5,
            fontface = "bold", color = "#3D3D3D") +
  # Thêm đường tham chiếu 50%
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  annotate("text", x = 2.4, y = 51.5,
           label = "50%", color = "gray50", size = 3.5) +
  scale_fill_manual(values = purchase_colors, name = "Trạng thái mua") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Tỷ Lệ Mua Hàng (Yes/No)",
    x = "Trạng thái",
    y = "Tỷ lệ (%)"
  ) +
  theme(
    plot.background    = element_rect(fill = "#FAFAFA", color = NA),
    panel.background   = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "#E07B54",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 12, color = "#777777"),
    axis.text.x   = element_text(size = 12, face = "bold"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: PHÂN PHỐI DAILY TIME SPENT
# ============================================

library(ggplot2)
library(readr)
library(dplyr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Tính mean và median
mean_time   <- mean(train$daily_time_spent, na.rm = TRUE)
median_time <- median(train$daily_time_spent, na.rm = TRUE)

# Vẽ biểu đồ
ggplot(train, aes(x = daily_time_spent)) +
  geom_histogram(binwidth = 20, fill = "#A8D8EA",
                 color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 20),
               color = "#5B84B1", linewidth = 0.8) +
  # Đường mean
  geom_vline(xintercept = mean_time, linetype = "dashed",
             color = "#D45E8A", linewidth = 0.8) +
  annotate("text", x = mean_time + 8, y = Inf, vjust = 1.5,
           label = paste0("Mean: ", round(mean_time, 1)),
           color = "#D45E8A", fontface = "bold", size = 3.8) +
  # Đường median
  geom_vline(xintercept = median_time, linetype = "dotted",
             color = "#E07B54", linewidth = 0.8) +
  annotate("text", x = median_time + 8, y = Inf, vjust = 3.2,
           label = paste0("Median: ", round(median_time, 1)),
           color = "#E07B54", fontface = "bold", size = 3.8) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Phân Phối Thời Gian Sử Dụng MXH Hàng Ngày",
    subtitle = "Histogram · binwidth = 20 phút",
    x = "Thời gian sử dụng (phút/ngày)",
    y = "Số lượng"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    plot.margin   = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: PHÂN PHỐI FOMO INDEX (BIẾN TRUNG GIAN M)
# ============================================

library(ggplot2)
library(readr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Tính mean và median
mean_fomo   <- mean(train$fomo_index, na.rm = TRUE)
median_fomo <- median(train$fomo_index, na.rm = TRUE)

# Vẽ biểu đồ
ggplot(train, aes(x = fomo_index)) +
  geom_histogram(binwidth = 0.5, fill = "#FFD3A5",
                 color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 0.5),
               color = "#E07B54", linewidth = 0.8) +
  # Đường mean
  geom_vline(xintercept = mean_fomo, linetype = "dashed",
             color = "#D45E8A", linewidth = 0.8) +
  annotate("text", x = mean_fomo + 0.3, y = Inf, vjust = 1.5,
           label = paste0("Mean: ", round(mean_fomo, 2)),
           color = "#D45E8A", fontface = "bold", size = 3.8) +
  # Đường median
  geom_vline(xintercept = median_fomo, linetype = "dotted",
             color = "#5B84B1", linewidth = 0.8) +
  annotate("text", x = median_fomo + 0.3, y = Inf, vjust = 3.2,
           label = paste0("Median: ", round(median_fomo, 2)),
           color = "#5B84B1", fontface = "bold", size = 3.8) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Phân Phối FOMO Index",
    subtitle = "Biến trung gian M · binwidth = 0.5",
    x = "FOMO Index",
    y = "Số lượng"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    plot.margin   = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: BOXPLOT — BIG FIVE THEO PURCHASED
# ============================================

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Chuyển sang dạng long, chỉ giữ 3 trait quan trọng
big5_long <- train %>%
  select(purchased, Conscientiousness_Score,
         Neuroticism_Score, Openness_Score) %>%
  pivot_longer(-purchased,
               names_to  = "Trait",
               values_to = "Score") %>%
  # Đổi tên trait cho đẹp
  mutate(Trait = recode(Trait,
                        "Conscientiousness_Score" = "Conscientiousness",
                        "Neuroticism_Score"       = "Neuroticism",
                        "Openness_Score"          = "Openness"
  ))

# Màu Yes/No
purchase_colors <- c("Yes" = "#B5EAD7", "No" = "#FFB7B2")

# Vẽ biểu đồ
ggplot(big5_long, aes(x = purchased, y = Score, fill = purchased)) +
  geom_boxplot(alpha = 0.9, width = 0.55,
               outlier.color = "gray50",
               outlier.size  = 1.5,
               outlier.alpha = 0.6) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3,
               fill = "white", color = "#3D3D3D") +
  facet_wrap(~Trait, scales = "free_y") +
  scale_fill_manual(values = purchase_colors, name = "Trạng thái mua") +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Phân Phối Big Five Traits Theo Hành Vi Mua Hàng",
    subtitle = "3 traits có ý nghĩa trong Logistic Regression · Điểm trắng = Mean",
    x = "Trạng thái mua",
    y = "Điểm số"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    strip.text    = element_text(face = "bold", size = 12,
                                 color = "#3D3D3D"),
    strip.background = element_rect(fill = "#F0F0F0", color = NA),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: CORRELATION HEATMAP
# ============================================

library(readr)
library(dplyr)
library(corrplot)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Chọn biến số và tính ma trận tương quan
numeric_vars <- train %>%
  select(where(is.numeric))

corr_matrix <- cor(numeric_vars, use = "complete.obs")

# Vẽ heatmap
corrplot(corr_matrix,
         method      = "color",
         type        = "upper",
         order       = "hclust",
         tl.cex      = 0.8,
         tl.col      = "#3D3D3D",
         tl.srt      = 45,
         addCoef.col = "#3D3D3D",
         number.cex  = 0.65,
         col         = colorRampPalette(c("#FFB7B2", "#FFFFFF", "#A8D8EA"))(200),
         outline     = FALSE,
         cl.cex      = 0.8,
         mar         = c(0, 0, 2, 0),
         title       = "Correlation Heatmap — Biến Số"
)

# ============================================
# EDA: SCATTER PLOT — FOMO INDEX VS PURCHASE PROB
# ============================================

library(ggplot2)
library(readr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Vẽ biểu đồ
ggplot(train, aes(x = fomo_index, y = purchase_prob, color = purchased)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(
    values = c("Yes" = "#4CAF82", "No" = "#D45E8A"),
    name   = "Trạng thái mua"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 1)) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "FOMO Index vs Xác Suất Mua Hàng",
    subtitle = "Phân nhóm theo trạng thái mua · Đường thẳng = Linear Trend",
    x = "FOMO Index",
    y = "Xác suất mua (purchase_prob)"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: DENSITY PLOT — PURCHASE PROB
# ============================================

library(ggplot2)
library(readr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Vẽ biểu đồ
ggplot(train, aes(x = purchase_prob, fill = purchased)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("Yes" = "#3D8DAE", "No" = "#E07B54"),
    name   = "Trạng thái mua"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  annotate("text", x = 0.52, y = Inf, vjust = 1.8,
           label = "Ngưỡng 0.5", color = "gray40",
           fontface = "bold", size = 3.8, hjust = 0) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Phân Phối Xác Suất Mua Hàng",
    subtitle = "Phân nhóm theo trạng thái mua · Đường kẻ = Ngưỡng 0.5",
    x = "Xác suất mua (purchase_prob)",
    y = "Mật độ"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )

# ============================================
# EDA: COVERT NARCISSISM → SOCIAL COMPARISON
#       → FOMO INDEX → MUA HÀNG
# ============================================

library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Hệ số tương quan
r1 <- cor(train$Covert_Narcissism, train$Social_Comparison, use = "complete.obs")
r2 <- cor(train$Social_Comparison, train$fomo_index,        use = "complete.obs")
r3 <- cor(train$fomo_index,        train$purchase_prob,     use = "complete.obs")

# Màu chung
col_yes <- "#2196F3"
col_no  <- "#F44336"

# 1. Covert Narcissism vs Social Comparison
p1 <- ggplot(train, aes(x = Covert_Narcissism, y = Social_Comparison,
                        color = purchased)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = c("Yes" = col_yes, "No" = col_no),
                     name = "Mua hàng") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(r1, 3)),
           hjust = 1.2, vjust = 1.8,
           size = 4.5, fontface = "bold", color = "#3D3D3D") +
  labs(title = "Covert Narcissism → Social Comparison",
       x = "Covert Narcissism", y = "Social Comparison") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# 2. Social Comparison vs FOMO Index
p2 <- ggplot(train, aes(x = Social_Comparison, y = fomo_index,
                        color = purchased)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = c("Yes" = col_yes, "No" = col_no),
                     name = "Mua hàng") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(r2, 3)),
           hjust = 1.2, vjust = 1.8,
           size = 4.5, fontface = "bold", color = "#3D3D3D") +
  labs(title = "Social Comparison → FOMO Index",
       x = "Social Comparison", y = "FOMO Index") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# 3. FOMO Index vs Purchase Prob
p3 <- ggplot(train, aes(x = fomo_index, y = purchase_prob,
                        color = purchased)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = c("Yes" = col_yes, "No" = col_no),
                     name = "Mua hàng") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(r3, 3)),
           hjust = 1.2, vjust = 1.8,
           size = 4.5, fontface = "bold", color = "#3D3D3D") +
  labs(title = "FOMO Index → Xác Suất Mua",
       x = "FOMO Index", y = "Purchase Prob") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5, size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "right")

# Ghép 3 biểu đồ lại
grid.arrange(p1, p2, p3, ncol = 3,
             top = grid::textGrob(
               "Chuỗi Tương Quan: Covert Narcissism → Social Comparison → FOMO → Mua Hàng",
               gp = grid::gpar(fontface = "bold", fontsize = 14, col = "#3D3D3D")
             )
)



# ============================================
# EDA: TUỔI + THU NHẬP + THỜI GIAN LƯỚT
# ============================================

library(ggplot2)
library(readr)
library(dplyr)

# Đọc dữ liệu
data_sma <- read_csv("C:/Data/Data/social_media_addiction.csv")
data_ads <- read_csv("C:/Data/Data/social_ads.csv")

# Tính trung bình thời gian lướt theo Age
avg_time <- data_sma %>%
  group_by(Age) %>%
  summarise(Daily_Usage_Time_min = mean(Daily_Usage_Time_min, na.rm = TRUE))

# Ghép 2 file theo Age
data_merge <- data_ads %>%
  left_join(avg_time, by = "Age") %>%
  filter(!is.na(Daily_Usage_Time_min))

# Vẽ scatter plot
ggplot(data_merge, aes(x = Age, y = EstimatedSalary,
                       color = Daily_Usage_Time_min)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "loess", color = "#555555",
              linetype = "twodash", se = FALSE,
              linewidth = 0.7) +
  scale_color_gradientn(
    colors = c("#A8D8EA", "#AA96DA", "#FCBAD3"),
    name   = "Thời gian lướt\n(phút/ngày)"
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Phân Tích EDA Đa Biến",
    subtitle = "Mối quan hệ giữa Tuổi, Thu nhập và Thời gian lướt MXH",
    x = "Độ Tuổi (Years)",
    y = "Thu nhập ước tính ($)"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    legend.title  = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.margin   = margin(20, 20, 15, 20)
  )


# ============================================
# EDA: PHÂN PHỐI 3 BIẾN
# ============================================

library(ggplot2)
library(readr)
library(gridExtra)

# Đọc dữ liệu
data_sma  <- read_csv("C:/Data/Data/social_media_addiction.csv")
data_ads  <- read_csv("C:/Data/Data/social_ads.csv")

# Màu pastel
col_age    <- "#A8D8EA"
col_income <- "#AA96DA"
col_time   <- "#FCBAD3"

# 1. Phân phối Tuổi (từ social_ads - nhiều dữ liệu hơn)
p1 <- ggplot(data_ads, aes(x = Age)) +
  geom_histogram(fill = col_age, color = "white", bins = 20, alpha = 0.85) +
  geom_density(aes(y = after_stat(count)), color = "#5B9EC9", linewidth = 0.8) +
  labs(title = "Phân phối Tuổi", x = "Tuổi", y = "Số lượng") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#3D3D3D"),
        panel.grid.minor = element_blank())

# 2. Phân phối Thu nhập
p2 <- ggplot(data_ads, aes(x = EstimatedSalary)) +
  geom_histogram(fill = col_income, color = "white", bins = 20, alpha = 0.85) +
  geom_density(aes(y = after_stat(count)), color = "#7B68C8", linewidth = 0.8) +
  labs(title = "Phân phối Thu nhập", x = "Thu nhập ước tính ($)", y = "Số lượng") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#3D3D3D"),
        panel.grid.minor = element_blank())

# 3. Phân phối Thời gian lướt MXH
p3 <- ggplot(data_sma, aes(x = Daily_Usage_Time_min)) +
  geom_histogram(fill = col_time, color = "white", bins = 15, alpha = 0.85) +
  geom_density(aes(y = after_stat(count)), color = "#D45E8A", linewidth = 0.8) +
  labs(title = "Phân phối Thời gian lướt MXH",
       x = "Thời gian (phút/ngày)", y = "Số lượng") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = "#3D3D3D"),
        panel.grid.minor = element_blank())

# Ghép 3 biểu đồ lại
grid.arrange(p1, p2, p3, ncol = 3,
             top = grid::textGrob(
               "EDA: Phân phối Tuổi · Thu nhập · Thời gian lướt MXH",
               gp = grid::gpar(fontface = "bold", fontsize = 15, col = "#3D3D3D")
             )
)

# ============================================
# EDA: TƯƠNG QUAN FOMO INDEX & FLOW EXPERIENCE
# ============================================

library(ggplot2)
library(readr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Tính hệ số tương quan
r_value <- cor(train$fomo_index, train$Flow_Experience, use = "complete.obs")

# Vẽ biểu đồ
ggplot(train, aes(x = fomo_index, y = Flow_Experience, color = purchased)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(
    values = c("Yes" = "#2196F3", "No" = "#F44336"),
    name   = "Trạng thái mua"
  ) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(r_value, 3)),
           hjust = 1.2, vjust = 1.8,
           size = 5, fontface = "bold", color = "#3D3D3D") +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Tương Quan FOMO Index và Flow Experience",
    x = "FOMO Index",
    y = "Flow Experience"
  ) +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )


# ============================================
# EDA: TỶ LỆ MUA THEO MỨC ĐỘ NGHIỆN
# ============================================

library(ggplot2)
library(readr)
library(dplyr)

# Đọc dữ liệu
data <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Tính tỷ lệ mua theo mức độ nghiện
addiction_purchase <- data %>%
  group_by(addiction_level_calc) %>%
  summarise(
    total  = n(),
    bought = sum(purchased == "Yes", na.rm = TRUE),
    rate   = bought / total * 100
  ) %>%
  arrange(desc(rate))

# Màu cho từng mức độ nghiện
addiction_colors <- c(
  "Low"      = "#A8D8EA",
  "Moderate" = "#B5EAD7",
  "High"     = "#FFD3A5",
  "Severe"   = "#FFB7B2"
)

# Vẽ biểu đồ
ggplot(addiction_purchase, aes(x = reorder(addiction_level_calc, rate),
                               y = rate,
                               fill = addiction_level_calc)) +
  geom_col(width = 0.55, alpha = 0.95) +
  geom_text(aes(label = paste0(round(rate, 1), "%")),
            hjust = -0.15, size = 4.5,
            fontface = "bold", color = "#3D3D3D") +
  scale_fill_manual(
    values = addiction_colors,
    name   = "Mức độ nghiện"
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(addiction_purchase$rate) * 1.15)) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Tỷ Lệ Mua Hàng Theo Mức Độ Nghiện MXH",
    subtitle = "Phân tích EDA · Dữ liệu: cleaned_research_train.csv",
    x = "Mức độ nghiện",
    y = "Tỷ lệ mua (%)"
  ) +
  theme(
    plot.background    = element_rect(fill = "#FAFAFA", color = NA),
    panel.background   = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    axis.text.y   = element_text(size = 11, face = "bold"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    legend.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.key        = element_rect(fill = "transparent"),
    plot.margin = margin(20, 30, 15, 20)
  )


# ============================================
# EDA: TỶ LỆ MUA HÀNG THEO PLATFORM
# ============================================

library(ggplot2)
library(readr)
library(dplyr)

# Đọc dữ liệu
train <- read_csv("C:/Data/Data/cleaned_research_train.csv")

# Màu cho từng platform
platform_colors <- c(
  "Discord"   = "#7B68C8",
  "Facebook"  = "#7EC8E3",
  "Instagram" = "#D45E8A",
  "LinkedIn"  = "#C7CEEA",
  "Pinterest" = "#FCBAD3",
  "Reddit"    = "#FFB7B2",
  "Snapchat"  = "#FFFACD",
  "TikTok"    = "#B5EAD7",
  "Twitter"   = "#E2F0CB",
  "YouTube"   = "#FFDAC1"
)

# Tính tỷ lệ mua theo platform
train %>%
  group_by(platform) %>%
  summarise(purchase_rate = mean(purchased == "Yes") * 100) %>%
  arrange(desc(purchase_rate)) %>%
  ggplot(aes(x = reorder(platform, purchase_rate),
             y = purchase_rate,
             fill = platform)) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_text(aes(label = paste0(round(purchase_rate, 1), "%")),
            hjust = -0.15, size = 4,
            fontface = "bold", color = "#3D3D3D") +
  scale_fill_manual(values = platform_colors) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(
    train %>% group_by(platform) %>%
      summarise(r = mean(purchased == "Yes") * 100) %>%
      pull(r)) * 1.15)) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Tỷ Lệ Mua Hàng Theo Nền Tảng",
    subtitle = "Phân tích EDA · Dữ liệu: cleaned_research_train.csv",
    x = "Nền tảng",
    y = "Tỷ lệ mua (%)"
  ) +
  theme(
    plot.background    = element_rect(fill = "#FAFAFA", color = NA),
    panel.background   = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ECECEC", linewidth = 0.5),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 15,
                                 color = "#3D3D3D", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray50",
                                 hjust = 0.5, margin = margin(b = 15)),
    axis.title.x  = element_text(face = "bold", size = 14,
                                 margin = margin(t = 15)),
    axis.title.y  = element_text(face = "bold", size = 14,
                                 margin = margin(r = 15)),
    axis.text     = element_text(size = 11, color = "#777777"),
    axis.text.y   = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(20, 30, 15, 20)
  )