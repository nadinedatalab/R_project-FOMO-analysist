# X = daily_time_spent
# M = fomo_index
# Y = purchase_prob
library(lavaan)

# Load data
train <- read.csv("data/cleaned_research_train.csv")

# Kiểm tra dữ liệu
summary(
  train[, c(
    "daily_time_spent",
    "fomo_index",
    "purchase_prob"
  )]
)

model_sem <- "

# Path a
fomo_index ~ a*daily_time_spent

# Path b và c'
purchase_prob ~ b*fomo_index + cprime*daily_time_spent

# Indirect effect
indirect := a*b

# Direct effect
direct := cprime

# Total effect
total := (a*b) + cprime

# Proportion mediated
prop_med := (a*b)/((a*b)+cprime)

"

# Fit SEM Model

fit_sem <- sem(
  model_sem,
  data = train
)

summary(
  fit_sem,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# Bootstrap CI
fit_sem_boot <- sem(
  model_sem,
  data = train,
  se = "bootstrap",
  bootstrap = 1000
)

# Extract mediation effects

mediation_results <- parameterEstimates(
  fit_sem_boot,
  boot.ci.type = "bca.simple",
  level = 0.95
)

mediation_results[
  mediation_results$label %in%
    c("indirect", "direct", "total", "prop_med"),
]