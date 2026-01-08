# Load packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(pROC)
library(MASS)

# Load data
accidents <- read.csv(
  "data/accidents_catalunya_english.xlsb.csv",
  stringsAsFactors = FALSE
)

# Create target + features
accidents <- accidents %>%
  mutate(
    dat = dmy(dat),
    post_toll = ifelse(dat >= as.Date("2021-09-01"), 1, 0),

    is_highway = via %in% c("AP-7", "AP-2", "C-32", "C-33"),
    weekend = ifelse(workday == "Weekend", 1, 0),
    night = ifelse(time == "Night", 1, 0),
    severe = ifelse(F_DEAD > 0 | F_SERIOUS_INJURIES > 0, 1, 0),
    month = month(dat),

    # Aggregated road-user involvement
    has_pedestrian = F_PEDESTRIAN_INVOLVED > 0,
    has_bike       = (F_BIKES_INVOLVED > 0) | (F_MOPED_INVOLVED > 0),
    has_motor      = F_MOTORCYCLES_INVOLVED > 0,
    has_heavy      = F_HEAVY_VEHICLES_INVOLVED > 0
  ) %>%
  mutate(
    zona = as.factor(zona),
    D_CLIMAT = as.factor(D_CLIMAT),
    D_ROAD_TYPE = as.factor(D_ROAD_TYPE),
    D_SURFACE = as.factor(D_SURFACE),
    D_LUMINOSITY = as.factor(D_LUMINOSITY),
    D_AT_INTERSECTION = as.factor(D_AT_INTERSECTION),
    C_ROAD_SPEED = as.numeric(C_ROAD_SPEED)   # keep numeric if it is numeric
  )

# Check imbalance
table(accidents$post_toll)
prop.table(table(accidents$post_toll))

# Select variables
vars_used <- c(
  "post_toll",
  "is_highway",
  "zona",
  "weekend",
  "night",
  "F_UNITS_INVOLVED",
  "severe",
  "D_CLIMAT",
  "D_ROAD_TYPE",
  "D_SURFACE",
  "D_LUMINOSITY",
  "D_AT_INTERSECTION",
  "C_ROAD_SPEED",
  "month",
  "has_pedestrian",
  "has_bike",
  "has_motor",
  "has_heavy"
)

acc_mod <- accidents[complete.cases(accidents[, vars_used]), ]

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

# Formulas
form_full <- post_toll ~
  is_highway + zona + weekend + night +
  F_UNITS_INVOLVED + severe +
  has_pedestrian + has_bike + has_motor + has_heavy +
  D_CLIMAT + D_ROAD_TYPE + D_SURFACE + D_LUMINOSITY + D_AT_INTERSECTION +
  C_ROAD_SPEED + factor(month)

form_manual <- post_toll ~ is_highway + zona + weekend +
  F_UNITS_INVOLVED + D_CLIMAT + D_ROAD_TYPE + factor(month)

# Null and full models (for stepwise)
mod_null <- glm(post_toll ~ 1, family = binomial, data = train, weights = w_train)
mod_full <- glm(form_full, family = binomial, data = train, weights = w_train)

# Stepwise AIC
mod_step <- stepAIC(mod_null,
                    scope = list(lower = mod_null, upper = mod_full),
                    direction = "both",
                    trace = TRUE)

# Manual model (baseline)
mod_manual <- glm(form_manual, family = binomial, data = train, weights = w_train)

# Residual plots (Deviance residuals)
par(mfrow = c(1,2))

plot(residuals(mod_step, type = "deviance"),
     main = "Deviance residuals",
     ylab = "Residual",
     xlab = "Observation")

abline(h = 0, col = "red")

plot(residuals(mod_step, type = "pearson"),
     main = "Pearson residuals",
     ylab = "Residual",
     xlab = "Observation")

abline(h = 0, col = "red")

par(mfrow = c(1,1))

# Influence check 
plot(cooks.distance(mod_step), type = "h",
     main = "Cook's Distance",
     ylab = "Influence")

abline(h = 4 / nrow(train), col = "red")

cd <- cooks.distance(mod_manual)
sum(cd > 4 / length(cd))
head(order(cd, decreasing = TRUE), 10)
sort(cd, decreasing = TRUE)[1:10]

# Evaluate on test set (AUC)
pred_manual <- predict(mod_manual, newdata = test, type = "response")
roc_manual  <- roc(test$post_toll, pred_manual)

pred_step <- predict(mod_step, newdata = test, type = "response")
roc_step  <- roc(test$post_toll, pred_step)

auc(roc_manual)
auc(roc_step)

plot(roc_manual, main = "ROC: Manual vs Stepwise")
plot(roc_step, add = TRUE)
legend("bottomright", legend = c("Manual", "Stepwise"),
       lty = 1, bty = "n")
