# Load packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(pROC)

# Load data
accidents <- read.csv(
  "data/accidents_catalunya_english.xlsb.csv",
  stringsAsFactors = FALSE
)
class(accidents)

#  Create target variable
accidents <- accidents %>%
  mutate(
    dat = dmy(dat),
    post_toll = ifelse(dat >= as.Date("2021-09-01"), 1, 0)
  )

# Check imbalance
table(accidents$post_toll)
prop.table(table(accidents$post_toll))

# Features
accidents <- accidents %>%
  mutate(
    is_highway = via %in% c("AP-7", "AP-2", "C-32", "C-33"),
    weekend = ifelse(workday == "Weekend", 1, 0),
    night = ifelse(time == "Night", 1, 0),
    severe = ifelse(F_DEAD > 0 | F_SERIOUS_INJURIES > 0, 1, 0)
  )

accidents <- accidents %>%
  mutate(
    zona = as.factor(zona),
    D_CLIMAT = as.factor(D_CLIMAT),
    D_ROAD_TYPE = as.factor(D_ROAD_TYPE),
    tipAcc = as.factor(tipAcc)
  )

# Remove rows with missing values 
vars_used <- c(
  "post_toll",
  "is_highway",
  "zona",
  "weekend",
  "night",
  "F_UNITS_INVOLVED",
  "severe",
  "D_CLIMAT",
  "D_ROAD_TYPE"
)

acc_mod <- accidents[complete.cases(accidents[, vars_used]), ]

# Add time structure feature
acc_mod <- acc_mod %>%
  mutate(
    month = month(dat)
  )

# Train/test split 
set.seed(123)

idx0 <- which(acc_mod$post_toll == 0)
idx1 <- which(acc_mod$post_toll == 1)

test_idx <- c(
  sample(idx0, size = floor(0.2 * length(idx0))),
  sample(idx1, size = floor(0.2 * length(idx1)))
)

test  <- acc_mod[test_idx, ]
train <- acc_mod[-test_idx, ]

# Use weights to handle class imbalance
w_train <- ifelse(train$post_toll == 1,
            sum(train$post_toll == 0) / sum(train$post_toll == 1),
            1)

# Fit a logistic regression
mod_logit <- glm(
  post_toll ~ is_highway +
              zona +
              weekend +
              F_UNITS_INVOLVED +
              D_CLIMAT +
              D_ROAD_TYPE +
              factor(month),
  family = binomial,
  data = train,
  weights = w_train
)

# Interpret the model 
summary(mod_logit)
exp(coef(mod_logit))
exp(confint(mod_logit))

# Evaluate model performance
pred_prob <- predict(mod_logit, newdata = test, type = "response")
roc_obj <- roc(test$post_toll, pred_prob)
plot(roc_obj)
auc(roc_obj)
