# =========================================
# CUSTOMER SEGMENTATION USING CLUSTERING
# =========================================

# Install packages if not installed
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("dplyr")

# Load libraries
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)

# =========================================
# LOAD DATASET
# =========================================

# Read CSV file
mall_data <- read.csv("C:/Users/Hello/Desktop/mall_data.csv")

# Display first rows
head(mall_data)

# Dataset structure
str(mall_data)

# Summary statistics
summary(mall_data)

# =========================================
# DATA PREPROCESSING
# =========================================

# Select important features for clustering
# Annual Income and Spending Score

data <- mall_data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

# Rename columns for easier use
colnames(data) <- c("Income", "Score")

# Check missing values
sum(is.na(data))

# Scale the data
scaled_data <- scale(data)

# =========================================
# ELBOW METHOD
# Determine Optimal Number of Clusters
# =========================================

wcss <- vector()

for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 25)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot Elbow Method
plot(1:10, wcss,
     type = "b",
     pch = 19,
     frame = FALSE,
     xlab = "Number of Clusters",
     ylab = "WCSS",
     main = "Elbow Method")

# =========================================
# K-MEANS CLUSTERING
# =========================================

# Create K-Means model with 5 clusters
set.seed(123)

kmeans_result <- kmeans(scaled_data,
                        centers = 5,
                        nstart = 25)

# Print clustering result
print(kmeans_result)

# Add cluster labels to dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# =========================================
# VISUALIZATION OF CLUSTERS
# =========================================

ggplot(data,
       aes(x = Income,
           y = Score,
           color = Cluster)) +
  geom_point(size = 4) +
  labs(title = "Customer Segmentation using K-Means",
       x = "Annual Income (k$)",
       y = "Spending Score") +
  theme_minimal()

# =========================================
# CLUSTER CENTERS
# =========================================

centers <- kmeans_result$centers

print("Cluster Centers:")
print(centers)

# =========================================
# SILHOUETTE SCORE
# =========================================

sil <- silhouette(kmeans_result$cluster,
                  dist(scaled_data))

# Plot silhouette
fviz_silhouette(sil)

# =========================================
# CLUSTER ANALYSIS
# =========================================

cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Income = mean(Income),
    Avg_Spending = mean(Score),
    Count = n()
  )

print(cluster_summary)

# =========================================
# SAVE RESULTS
# =========================================

write.csv(data,
          "Customer_Segmentation_Output.csv",
          row.names = FALSE)

print("Clustering Completed Successfully!")
