#Implmentation of kmean clustering using iris dataset.
#analyze and predict data for testing set





# Load necessary libraries
library(datasets)   # For built-in datasets
#install.packages("caret")
#install.packages("caret")

library(caret)      # For data splitting
library(cluster)    # For clustering functions

# Load the Iris dataset
data("iris")

# View the first few rows of the dataset
head(iris)




# Remove the Species column for clustering
iris_data <- iris[, -5]  # Exclude the 5th column (Species)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)

train_data <- iris_data[trainIndex, ]
test_data <- iris_data[-trainIndex, ]



# Apply K-means clustering
set.seed(123)  # For reproducibility
kmeans_model <- kmeans(train_data, centers = 3, nstart = 20)

# View the K-means clustering result
print(kmeans_model)

# Add cluster assignments to the training data
train_clusters <- data.frame(train_data, Cluster = kmeans_model$cluster)

# Print a few rows of clustered data
head(train_clusters)




# Function to assign clusters based on distance to centers
predict_clusters <- function(test_data, centers) {
  apply(test_data, 1, function(x) {
    distances <- apply(centers, 1, function(center) sqrt(sum((x - center)^2)))
    return(which.min(distances))
  })
}

# Predict clusters for the testing set
test_clusters <- predict_clusters(test_data, kmeans_model$centers)

# Add predicted clusters to testing data
test_clusters_df <- data.frame(test_data, Predicted_Cluster = test_clusters)

# Print testing data with predicted clusters
head(test_clusters_df)



# Plot clusters for training data
pairs(train_data, col = kmeans_model$cluster, main = "K-means Clustering on Training Set")

# Plot predicted clusters for testing data
pairs(test_data, col = test_clusters, main = "Predicted Clusters for Testing Set")


# Add true labels for comparison
test_clusters_df$True_Species <- iris$Species[-trainIndex]

# View comparison
print(test_clusters_df)

# Confusion matrix to compare clusters and species
table(Predicted_Cluster = test_clusters_df$Predicted_Cluster, True_Species = test_clusters_df$True_Species)

