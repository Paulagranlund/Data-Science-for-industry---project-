# Load libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(ggpubr)
library(sf)       
library(naniar)
library(patchwork)
library(caret)
library(forcats) # For factor manipulation
library(rpart)
library(rpart.plot)

# Data load
df <- read_csv(file = "data/accidents_catalunya_english.xlsb.csv")

summary(df)

# Investigating the C_ROAD_SPEED variable
summary(df$C_ROAD_SPEED)
boxplot(df$C_ROAD_SPEED, main = "C_ROAD_SPEED")

# Imputing the value for the observations with 999 speed limit
df <- df %>%
  mutate(
    C_ROAD_SPEED = if_else(C_ROAD_SPEED == 999, NA_real_, C_ROAD_SPEED)
  )
df <- df %>%
  mutate(
    C_ROAD_SPEED = ifelse(
      is.na(C_ROAD_SPEED),
      median(C_ROAD_SPEED, na.rm = TRUE),
      C_ROAD_SPEED
    )
  )
summary(df$C_ROAD_SPEED)

# Format date column
df$dat <- as.Date(df$dat, format = "%d/%m/%Y")

# add before/after variable for law change on 1st September 2021
df <- df %>%
  mutate(post_toll = if_else(dat < ymd("2021-09-01"), 
                                "Before", "After"))

# count missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

# remove columns containing more than 40% missing values
threshold <- nrow(df) * 0.4
df_cleaned <- df[, colSums(is.na(df)) <= threshold]

# Count missing values again
missing_values <- sapply(df_cleaned, function(x) sum(is.na(x)))
print(missing_values) # Removes D_PRIORITY_REGULATION, D_SUBTYPE_STREET,D_ROAD_OWNERSHIP, D_ROAD_LAYOUT

# Drop date, time, pk, Any and via column
df_cleaned <- df_cleaned %>%
  select(-dat, -time, -via, -Any, -pk)

# Converting character columns to factors
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.character), as.factor))

# Ensure integer columns are numeric
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.integer), as.numeric))

# Imputing missing values for categorical columns, except the target variable
df_cleaned <- df_cleaned %>%
  mutate(
    across(
      where(is.factor) & !all_of("post_toll"),
      ~ fct_na_value_to_level(.x, level = "Unknown")
    )
  )
# Imputing missing values for numeric columns
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Check missing values again
missing_values <- sapply(df_cleaned, function(x) sum(is.na(x)))
print(missing_values)

# Split dataq into training and testing sets
set.seed(123)

train_index <- createDataPartition(df_cleaned$post_toll, p = 0.7, list = FALSE)
train_data <- df_cleaned[train_index, ]
test_data <- df_cleaned[-train_index, ]

# Fit tree
tree_model <- rpart(
  post_toll ~ .,
  data = train_data,
  method = "class",
  parms = list(
    prior = c(0.5, 0.5)  # force equal importance of classes
  ),
  control = rpart.control(cp = 0.001, minsplit = 20))
# Plot the tree
rpart.plot(tree_model, type = 3, extra = 101)

### Evaluation of model
# Predict on test data
pred <- predict(tree_model, newdata = test_data, type = "class")
# Confusion matrix
conf_mat <- confusionMatrix(pred, test_data$post_toll)
conf_mat
# Variable importance
tree_model$variable.importance

## Improvemnets
# Tuning the complexity parameter (cp) to prune the tree
printcp(tree_model)
plotcp(tree_model)

best_cp <- tree_model$cptable[
  which.min(tree_model$cptable[, "xerror"]),
  "CP"
]

pruned_tree <- prune(tree_model, cp = best_cp)

# Re-evaluate the pruned tree
# Plot the tree
rpart.plot(pruned_tree, type = 3, extra = 101)
### Evaluation of model
# Predict on test data
pred_pruned <- predict(pruned_tree, newdata = test_data, type = "class")
# Confusion matrix
conf_mat <- confusionMatrix(pred_pruned, test_data$post_toll)
conf_mat
# Variable importance
pruned_tree$variable.importance

# Visualize the variable importance
var_imp <- tree_model$variable.importance

vi_df <- data.frame(
  variable = names(var_imp),
  importance = as.numeric(var_imp)
) %>%
  arrange(desc(importance)) %>%
  slice(1:5)

ggplot(vi_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance from Classification Tree",
    x = "Variable",
    y = "Relative Importance"
  ) +
  theme_minimal()

## the pruning did not improve the model significantly