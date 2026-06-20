# R_project-FOMO-analysist
# Phân Tích Hành Vi Tiêu Dùng Trên Mạng Xã Hội
## FOMO & Impulsive Buying Behavior — S-O-R Framework

## Mô tả dự án
Nghiên cứu thực chứng phân tích tác động của hội chứng sợ bỏ lỡ (FOMO) 
đến hành vi mua sắm bốc đồng của người dùng mạng xã hội tại Việt Nam, 
sử dụng khung lý thuyết Stimulus–Organism–Response (S-O-R).

## Dataset
- Nguồn: Kaggle — Social Media Addiction & Usage Patterns
- 10.000 quan sát (8.000 train / 2.000 test)
- 22 biến: hành vi MXH, Big Five Personality, FOMO, Flow Experience, Scarcity Exposure

## Phương pháp phân tích
| Mô hình | Mục tiêu | Kết quả chính |
|---|---|---|
| Path Analysis (lavaan) | Kiểm định cơ chế nhân quả FOMO | Prop. Mediated = 61.9% |
| Logistic Regression | Dự báo hành vi mua hàng | Accuracy 84.3%, AUC 0.896 |
| K-Means Clustering | Phân khúc khách hàng | K=4 phân khúc |
| Random Forest | Mô hình đối chứng ML | Accuracy 74.5% |

## Kết quả chính
- **61.9%** tác động của thời gian lướt MXH lên hành vi mua hàng 
  được dẫn truyền qua FOMO
- **Conscientiousness** là yếu tố bảo vệ mạnh nhất (OR = 0.025) 
  — tính tận tâm cao giảm 97.5% khả năng mua bốc đồng
- **FOMO** là yếu tố thúc đẩy mạnh nhất (OR = 1.591)
- **4 phân khúc khách hàng:**
  - FOMO Buyer — xác suất mua 90.7%
  - Engaged Shopper — xác suất mua 79.4%
  - Casual Browser — xác suất mua 67.8%
  - Rational Saver — xác suất mua 43.5%

## Cấu trúc thư mục
FOMO_analysist/

├── data/               # Dữ liệu train và test (không push lên GitHub)

├── scripts/

│   ├── 01_eda.R        # Phân tích khám phá dữ liệu (EDA)

│   ├── 02_sem_lavaan.R # Path Analysis & Mediation

│   ├── 03_logistic_regression.R  # Logistic Regression

│   ├── 04_kmeans_clustering.R    # K-Means Clustering

│   └── 05_random_forest.R        # Random Forest

├── output/             # Biểu đồ xuất ra (PNG)

├── FOMO_project.Rmd    # Báo cáo tổng hợp

└── README.md


