#Subtask 1

library(readxl)
library(cluster)
library(NbClust)
library(factoextra)


# Load the dataset
vehicles <- read_excel("vehicles.xlsx")

# Visualize outliers using boxplots for each variable separately
par(mfrow=c(3,6)) # Set the layout of the plots
for (i in 2:19) {
  boxplot(vehicles[,i], main=paste(colnames(vehicles)[i]))
}

# Outlier detection/removal using the Z-score method
z_scores <- apply(vehicles[,2:19], 2, function(x) abs(scale(x)))
vehicles <- vehicles[rowSums(z_scores < 3) == ncol(z_scores),]

# Scaling using Min-Max scaling
scaled_vehicles <- apply(vehicles[,2:19], 2, function(x) (x - min(x)) / (max(x) - min(x)))

# NBclust
nb <- NbClust(scaled_vehicles, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Elbow method
fviz_nbclust(scaled_vehicles, kmeans, method = "wss")

# Gap statistics method
gap_stat <- clusGap(scaled_vehicles, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Silhouette method
fviz_nbclust(scaled_vehicles, kmeans, method = 'silhouette')

# Perform k-means clustering for k=2
k <- 2 # Set the most favored "k" based on the automated methods
kmeans_fit <- kmeans(scaled_vehicles, centers = k, nstart = 25)

# Print the k-means output
print(kmeans_fit)

# Plot the clusters
fviz_cluster(kmeans_fit, data = scaled_vehicles, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

# Calculate the BSS and WSS indices
BSS <- sum(kmeans_fit$size * apply(kmeans_fit$centers, 1, function(x) sum((x - mean(scaled_vehicles))^2)))
TSS <- sum(apply(scaled_vehicles, 2, function(x) sum((x - mean(scaled_vehicles))^2)))
WSS <- TSS - BSS

# Print the BSS, WSS, and BSS/TSS ratio
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")
cat("BSS/TSS Ratio:", BSS/TSS, "\n")

# Perform k-means clustering for k=3
k <- 3 # Set the most favored "k" based on the automated methods
kmeans_fit <- kmeans(scaled_vehicles, centers = k, nstart = 25)

# Print the k-means output
print(kmeans_fit)

# Plot the clusters
fviz_cluster(kmeans_fit, data = scaled_vehicles, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

# Calculate the BSS and WSS indices
BSS <- sum(kmeans_fit$size * apply(kmeans_fit$centers, 1, function(x) sum((x - mean(scaled_vehicles))^2)))
TSS <- sum(apply(scaled_vehicles, 2, function(x) sum((x - mean(scaled_vehicles))^2)))
WSS <- TSS - BSS

# Print the BSS, WSS, and BSS/TSS ratio
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")
cat("BSS/TSS Ratio:", BSS/TSS, "\n")

# Create a silhouette plot
sil <- silhouette(kmeans_fit$cluster, dist(scaled_vehicles))
fviz_silhouette(sil)

# Calculate the average silhouette width score
avg_sil_width <- mean(sil[,3])

# Print the average silhouette width score
cat("Average Silhouette Width Score:", avg_sil_width, "\n")



#SubTask 2

library(readxl)
library(NbClust)
library(factoextra)
library(cluster)
library(ggplot2)

# Load the dataset
vehicles <- read_excel("vehicles.xlsx")

# Visualize outliers using boxplots for each variable separately
par(mfrow=c(3,6)) # Set the layout of the plots
for (i in 2:19) {
  boxplot(vehicles[,i], main=paste(colnames(vehicles)[i]))
}

# Outlier detection/removal using the Z-score method
z_scores <- apply(vehicles[,2:19], 2, function(x) abs(scale(x)))
vehicles <- vehicles[rowSums(z_scores < 3) == ncol(z_scores),]

# Scaling using Min-Max scaling
scaled_vehicles <- apply(vehicles[,2:19], 2, function(x) (x - min(x)) / (max(x) - min(x)))

# Perform PCA
pca_result <- prcomp(scaled_vehicles, center = TRUE, scale = TRUE)

# Display eigenvalues, eigenvectors, and cumulative score
summary(pca_result)

# Choose PCs with a cumulative score > 92%
cumulative_score <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
selected_pcs <- which(cumulative_score >= 0.92)[1]

# Create a new transformed dataset with selected PCs
vehicles_pca <- predict(pca_result, newdata = scaled_vehicles)[,1:selected_pcs]


# NbClust
nbclust_result_pca <- NbClust(vehicles_pca, min.nc = 2, max.nc = 10, method = "kmeans")
best_k_nbclust_pca <- nbclust_result_pca$Best.nc[1]

# Elbow method
set.seed(123)
fviz_nbclust(vehicles_pca, kmeans, method = "wss")

# Gap statistics
set.seed(123)
fviz_nbclust(vehicles_pca, kmeans, method = 'gap_stat')

# Silhouette method
silhouette_scores_pca <- sapply(2:10, function(k) {
  set.seed(123)
  cluster_result_pca <- kmeans(vehicles_pca, centers = k)
  silhouette_avg_width_pca <- mean(silhouette(cluster_result_pca$cluster, dist(vehicles_pca))[, "sil_width"])
  return(silhouette_avg_width_pca)
})
plot(2:10, silhouette_scores_pca, type = "b", xlab = "Number of clusters", ylab = "Average silhouette width", main = "Silhouette method")
best_k_silhouette_pca <- which.max(silhouette_scores_pca)

#Perform k-means clustering with the most favored k from the automated methods:

k <- 3
set.seed(123)
kmeans_result_pca <- kmeans(vehicles_pca, centers = k)

# Display k-means output, including centers, clustered results, and BSS/TSS ratio
kmeans_result_pca
WSS_pca <- sum(kmeans_result_pca$tot.withinss)
TSS_pca <- sum(kmeans_result_pca$totss)
BSS_pca <- TSS_pca - WSS_pca
BSS_ratio <- BSS_pca / TSS_pca

# Plot the clusters
fviz_cluster(kmeans_result_pca, data = scaled_vehicles, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

# Print the BSS, WSS, and the ratio of BSS over TSS
cat("BSS:", BSS_pca, "\n")
cat("WSS:", WSS_pca, "\n")
cat("Ratio of BSS over TSS:", BSS_ratio, "\n")

# Provide the silhouette plot and average silhouette width score for the new k-means attempt:
sil_pca <- silhouette(kmeans_result_pca$cluster, dist(vehicles_pca))
fviz_silhouette(sil_pca)
avg_sil_width_pca <- mean(sil_pca[, "sil_width"])
cat("Average Silhouette Width Score:", avg_sil_width_pca, "\n")

# Calculate Calinski-Harabasz Index
calinski_harabasz_pca <- function(cluster_result, data) {
  k <- length(unique(cluster_result$cluster))
  n <- nrow(data)
  BSS <- cluster_result$betweenss
  WSS <- cluster_result$tot.withinss
  
  ch_index <- ((n - k) / (k - 1)) * (BSS / WSS)
  return(ch_index)
}

ch_index_pca <- calinski_harabasz_pca(kmeans_result_pca, vehicles_pca)
ch_index_pca