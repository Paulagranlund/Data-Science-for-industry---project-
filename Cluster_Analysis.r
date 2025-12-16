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
library(leaps) #needed for the best subset modeling
library(clustMixType)
library(caret)
library(forcats) # For factor manipulation

# Data load
df <- read_csv(file = "data/accidents_catalunya_english.xlsb.csv")

summary(df)

# count missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

# remove columns containing more than 10% missing values
threshold <- nrow(df) * 0.1
df_cleaned <- df[, colSums(is.na(df)) <= threshold]

# Count missing values again
missing_values <- sapply(df_cleaned, function(x) sum(is.na(x)))
print(missing_values) # Removes D_TERRAIN_CHARACTER, D_INFL_FOG, C_ROAD_SPEED, D_INFL_WIND, D_PRIORITY_REGULATION, D_SUBTYPE_STREET,D_ROAD_OWNERSHIP, D_ROAD_LAYOUT, D_TRAFFIC_DIRRECTIONS

# Use KMeans clustering to identify patterns in accident data
# Add an ID column for reference
#df_cleaned$id <- seq_len(nrow(df_cleaned))

# Drop date, time and via column, as they have no meaningful semantic for clustering
df_cleaned <- df_cleaned %>%
  select(-dat, -time, -via, -pk)

summary(df_cleaned)

# Convert character variables to factors and integer to numeric
df_kproto <- df_cleaned %>%
  mutate(across(where(is.character), as.factor))

df_kproto <- df_kproto %>%
  mutate(across(where(is.integer), as.numeric))

df_kproto <- df_kproto %>% drop_na()

# Scaling numeric variables
num_vars <- df_kproto %>% select(where(is.numeric))
cat_vars <- df_kproto %>% select(where(is.factor))
num_scaled <- scale(num_vars)
# Cap scaled values to be within -5 to 5 to limit the influence of outliers
num_scaled[num_scaled > 5] <- 5
num_scaled[num_scaled < -5] <- -5

df_kproto_scaled <- bind_cols(
  as.data.frame(num_scaled),
  cat_vars
)
summary(df_kproto_scaled)

# Lumping infrequent categories in categorical variables
df_kproto_scaled <- df_kproto_scaled %>%
  mutate(
    nomMun = fct_lump_min(nomMun, min = 300),
    nomCom = fct_lump_min(nomCom, min = 700),
    D_SUBTYPE_ACCIDENT = fct_lump_min(D_SUBTYPE_ACCIDENT, min = 1800)
  )

summary(df_kproto_scaled)


# Run K-Prototypes clustering
# Model selection: Elbow method
wss <- numeric(10)

for (k in 2:10) {
  fit <- kproto(df_kproto_scaled, k = k)
  wss[k] <- fit$tot.withinss
}

plot(2:10, wss[2:10], type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total Within-Cluster Cost",
     main = "Elbow Method for K-Prototypes")

# Using k = 7 based on elbow method
set.seed(123)

kproto_fit <- kproto(
  df_kproto_scaled,
  k = 7,          # your chosen number of clusters
  lambda = NULL   # automatic weighting (recommended)
)

df_kproto_scaled$cluster <- as.factor(kproto_fit$cluster)

# Inspect cluster sizes
table(df_kproto_scaled$cluster)

# Inspect cluster centers
kproto_fit$centers

# Total within-cluster sum of squares
kproto_fit$tot.withinss

#Comparing to baseline model
set.seed(123)
k1 <- kproto(df_kproto_scaled, k = 1)
k1$tot.withinss
1 - (kproto_fit$tot.withinss / k1$tot.withinss) # Since the improvement is less than 33,7%, the clustering is not very effective.


# Analyzing clusters
cluster_summary <- df_kproto_scaled %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ if(is.numeric(.)) mean(.) else as.character(names(sort(table(.), decreasing = TRUE)[1]))))

cluster_summary
print(cluster_summary)
# Visualizing clusters
ggplot(df_kproto_scaled, aes(x = F_DEAD, fill = cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Fatalities by Cluster",
       x = "Fatalities",
       y = "Count") +
  theme_minimal()

# Checking clustering stability
set.seed(1)
costs <- replicate(10, kproto(df_kproto_scaled, k = 5)$tot.withinss)
sd(costs) / mean(costs) # 3% within-cluster variation, indicating stable clustering
