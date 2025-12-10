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
library(caret)
library(cluster) # for pam clustering

# Data load
df <- read_csv(file = "data/accidents_catalunya_english.xlsb.csv")

summary(df)

# count missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

# remove columns containing more than 40% missing values
threshold <- nrow(df) * 0.4
df_cleaned <- df[, colSums(is.na(df)) <= threshold]

# Count missing values again
missing_values <- sapply(df_cleaned, function(x) sum(is.na(x)))
print(missing_values)

# Use KMeans clustering to identify patterns in accident data
# Add an ID column for reference
df_cleaned$id <- seq_len(nrow(df_cleaned))

# Select relevant numerical columns for clustering
numerical_data <- df_cleaned %>%
  select(id, where(is.numeric)) %>%
  drop_na()

summary(numerical_data)

# Scale the data
numerical_data_scaled <- scale(numerical_data)

# Determine optimal number of clusters using the elbow method
wss <- numeric(10)
for (k in 1:10) {
  kmeans_model <- kmeans(numerical_data_scaled, centers = k, nstart = 10)
  wss[k] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
elbow_plot <- ggplot(data = data.frame(Clusters = 1:10, WSS = wss), aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Within-Cluster Sum of Squares")

print(elbow_plot)

# From the elbow plot, choose the optimal number of clusters
optimal_clusters <- 8

# Apply KMeans clustering
set.seed(123)
kmeans_final <- kmeans(numerical_data_scaled, centers = optimal_clusters, nstart = 10)

cluster_assignments <- data.frame(
  id = numerical_data$id,
  cluster = kmeans_final$cluster
)

df_cleaned <- df_cleaned %>%
  left_join(cluster_assignments, by = "id")

# Visualize clusters using PCA
pca_result <- prcomp(numerical_data_scaled, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_result$x[, 1:2], cluster = as.factor(kmeans_final$cluster))

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA of KMeans Clusters", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Size of clusters
table(df_cleaned$cluster)

# Convert cluster column to factor
df_cleaned$cluster <- as.factor(df_cleaned$cluster)

# View cluster centers
kmeans_final$centers
# View unscaled cluster centers
unscaled_centers <- scale(kmeans_final$centers,
                          center = -attr(numerical_data_scaled, "scaled:center"),
                          scale  = 1 / attr(numerical_data_scaled, "scaled:scale"))

unscaled_centers

# Summarize clusters
df_cleaned %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))


# Now handling categorical variables as well
df_cleaned_fixed <- df_cleaned %>%
  mutate(across(where(is.character), as.factor))

str(df_cleaned_fixed)
gower_dist <- daisy(df_cleaned_fixed, metric = "gower")

pam_fit <- pam(gower_dist, k = 8)

df_cleaned$cluster <- pam_fit$cluster

# Size of clusters
table(df_cleaned$cluster)
