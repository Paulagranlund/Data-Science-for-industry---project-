# libs --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(leaps)
library(randomForest)
library(caret)
library(tree)
library(tidymodels)
library(pROC)
library(smotefamily)
library(ROSE)
# init sets -----------------------------------------------------------
data <- miniDf
# Init set
set.seed(123)
sample_size <- floor(0.70 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
initT_set <- data[train_indices, ]
test_set <- data[-train_indices, ]

table(initT_set$IS_after_toll)
table(test_set$IS_after_toll)

# Formula -----------------------------------------------------------------
formula <- formula(IS_after_toll ~ zona + nomDem + F_UNITS_INVOLVED + F_PEDESTRIAN_INVOLVED + 
                     F_BIKES_INVOLVED + F_MOPED_INVOLVED + F_MOTORCYCLES_INVOLVED + 
                     F_LIGHT_VEHICLES_INVOLVED + F_HEAVY_VEHICLES_INVOLVED + C_ROAD_SPEED + 
                     D_FOG + D_INFL_TRAFFIC + D_INFL_CLIMAT + D_INFL_SPECIAL_TRAFFIC + 
                     D_INFL_LOW_VISIBILITY + IS_on_toll_road + month + IS_fatal)



# Oversampled Set ---------------------------------
true_class <- initT_set[initT_set$IS_after_toll == TRUE, ]
false_class <- initT_set[initT_set$IS_after_toll == FALSE, ]

true_oversampled <- true_class[sample(nrow(true_class), nrow(false_class) + 1000, replace = TRUE), ]

oversampled_set <- rbind(false_class, true_oversampled)
table(oversampled_set$IS_after_toll)

# initT RF ---------------------------------------------------------------
ctrl <- trainControl(method = "cv", 
                     number = 8, 
                     verboseIter = TRUE
)
rf <- randomForest(formula, data = initT_set, mtry = 5, ntree = 1000, do.trace = TRUE)

test_set$rf <- predict(rf, newdata = test_set)
roc_init <- roc(test_set$IS_after_toll, test_set$rf)
plot(roc_init)

# Oversampled RF ----------------------------------------------------------
rf_over <- randomForest(formula, data = oversampled_set, mtry = 5, ntree = 1000, do.trace = TRUE)

test_set$rf_over <- predict(rf_over, newdata = test_set)
roc_over <- roc(test_set$IS_after_toll, test_set$rf_over)
plot(roc_over)

# K-fold RF ---------------------------------------------------------------
ctrl <- trainControl(method = "cv", 
                     number = 8, 
                     verboseIter = TRUE
                     )

initT_set$IS_after_toll <- as.factor(initT_set$IS_after_toll)
startDf$IS_after_toll <- as.factor(startDf$IS_after_toll)

rf_fold <- train(formula,
                 data = initT_set, 
                 method = "rf", 
                 trControl = ctrl,
                 ntree = 500, 
                 importance = TRUE,
                 )

test_set$rf_fold <- predict(rf_fold, newdata = test_set, type = "prob")
roc_fold <- roc(test_set$IS_after_toll, test_set$rf_fold[,"TRUE"])
plot(roc_fold)
summary(roc_fold)

best_fold <- coords(roc_fold, "best", ret = c("threshold", "specificity", "sensitivity", "accuracy"))
best_init <- coords(roc_init, "best", ret = c("threshold", "specificity", "sensitivity", "accuracy"))
best_over <- coords(roc_over, "best", ret = c("threshold", "specificity", "sensitivity", "accuracy", ))

rf_fold_whole <- train(formula,
                 data = startDf, 
                 method = "rf", 
                 trControl = ctrl,
                 ntree = 500, 
                 importance = TRUE,
) 

test_set$rf_fold <- predict(rf_fold, newdata = test_set, type = "prob")
roc_fold <- roc(test_set$IS_after_toll, test_set$rf_fold[,"TRUE"])
plot(roc_fold)


# nalasys ----------------------------------------------------------------

roc_list <- list(BaseLine = roc_init, 
                 Oversampled = roc_over, 
                 Kfold = roc_fold)

ggroc(roc_list, size = 1) +
  theme_minimal() +
  scale_color_manual(values = c("orange", "steelblue", "darkgreen")) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "Comparison of 3 ROC Models",
       color = "Model Type",
       x = "Specificity",
       y = "Sensitivity")

best_std   <- coords(roc_init,   "best", ret = c("threshold", "sensitivity", "specificity"))
best_kfold <- coords(roc_fold, "best", ret = c("threshold", "sensitivity", "specificity"))
best_over  <- coords(roc_over,  "best", ret = c("threshold", "sensitivity", "specificity"))

startDf$rf_fold <- predict(rf_fold, newdata = startDf, type = "prob")
startDf$rf_init <- predict(rf, newdata = startDf)
startDf$rf_over <- predict(rf_over, newdata = startDf)

roc_fW <- roc(startDf$IS_after_toll, startDf$rf_fold[,"TRUE"])
roc_iW <- roc(startDf$IS_after_toll, startDf$rf_init)
roc_oW <- roc(startDf$IS_after_toll, startDf$rf_over)


roc_list2 <- list(Oversampled = roc_oW, 
                 Training = roc_iW, 
                 Kfold = roc_fW)

ggroc(roc_list2, size = 1) +
  theme_minimal() +
  scale_color_manual(values = c("orange", "steelblue", "darkgreen")) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "Comparison of 3 ROC Models",
       color = "Model Type",
       x = "Specificity",
       y = "Sensitivity")



# Importance analasys -----------------------------------------------------
imp_data <- varImp(rf_fold, scale = TRUE)
D_FOG                        100.000
C_ROAD_SPEED                  60.944
F_MOPED_INVOLVED              29.706
F_BIKES_INVOLVED              29.016
month                         26.549
nomDem                        20.927
F_LIGHT_VEHICLES_INVOLVED     27.980
F_UNITS_INVOLVED              18.866

rf$importance
C_ROAD_SPEED                  71.434921
month                        113.679830
F_LIGHT_VEHICLES_INVOLVED     50.113505
F_UNITS_INVOLVED              45.832030
D_FOG                         33.091262
F_BIKES_INVOLVED              18.704538
F_MOPED_INVOLVED              11.464419
nomDem                        44.941782

rf_over$importance
month                        492.517510
C_ROAD_SPEED                 287.259016
F_UNITS_INVOLVED             182.254420
nomDem                       197.331034
D_FOG                        137.393846
F_LIGHT_VEHICLES_INVOLVED    194.208891
F_BIKES_INVOLVED              69.938057
F_MOPED_INVOLVED              53.083737

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Create the data frame from your provided values
imp_data <- data.frame(
  Feature = c("D_FOG", "C_ROAD_SPEED", "F_MOPED_INVOLVED", "F_BIKES_INVOLVED", 
              "month", "nomDem", "F_LIGHT_VEHICLES_INVOLVED", "F_UNITS_INVOLVED"),
  
  K_Fold = c(100.000, 60.944, 29.706, 29.016, 26.549, 20.927, 27.980, 18.866),
  
  RF = c(33.091, 71.435, 11.464, 18.705, 113.680, 44.942, 50.114, 45.832),
  
  RF_Over = c(137.394, 287.259, 53.084, 69.938, 492.518, 197.331, 194.209, 182.254)
)

# 2. Normalize scores to 0-100 (Relative Importance)
# This ensures we are comparing the "Rank", not the raw scale
normalize <- function(x) (x / max(x)) * 100

imp_norm <- imp_data %>%
  mutate(across(c(K_Fold, RF, RF_Over), normalize))

# 3. Transform to "Long" format for ggplot and pick top 6 based on average importance
plot_data <- imp_norm %>%
  pivot_longer(cols = -Feature, names_to = "Model", values_to = "Importance") %>%
  group_by(Feature) %>%
  mutate(avg_imp = mean(Importance)) %>%
  ungroup() %>%
  arrange(desc(avg_imp)) %>%
  slice_head(n = 18) # 6 variables * 3 models = 18 rows

# 4. Create the Overlapping (Grouped) Bar Plot
ggplot(plot_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("K_Fold" = "#999999", 
                               "RF" = "#E69F00", 
                               "RF_Over" = "#56B4E9")) +
  labs(title = "Comparison of Top 6 Important Variables",
       subtitle = "Scaled to Relative Importance (0-100%)",
       x = "Features",
       y = "Importance (Normalized %)") +
  theme_minimal() +
  theme(legend.position = "top")
