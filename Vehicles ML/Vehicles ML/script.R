library(readxl)
library(cluster)
library(factoextra)
library(NbClust)
library(caret)
library(factoextra)

# Load the dataset
data <- read_excel("vehicles.xlsx")

# Identify outliers using z-score method
z_scores <- apply(data[,1:18], 2, function(x) abs(scale(x)))
outliers <- which(z_scores > 3, arr.ind = TRUE)

# Visualize outliers using boxplots for each variable separately
par(mfrow=c(3,6)) # Set the layout of the plots
for (i in 1:18) {
  boxplot(data[,i], main=paste(colnames(data)[i]))
}

# Remove outliers from the dataset
data <- data[outliers[,1],]

# Scale the dataset using min-max  normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalized_data <- as.data.frame(lapply(data[,1:18], normalize))

# View the normalized dataset
head(normalized_data)

# Remove highly correlated variables
cor_matrix <- cor(normalized_data)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
reduced_data <- normalized_data[,-high_cor]

# Nbclust method
nbclust_result <- NbClust(reduced_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
cat("Optimal number of clusters according to NbClust:", nbclust_result$Best.nc[1], "\n")


# Elbow method
fviz_nbclust(reduced_data, kmeans, method = "wss") + 
geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "Elbow method")

# Gap statistics method
gap_stat <- clusGap(reduced_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Silhouette method
silhouette_scores <- numeric(0)
for (k in 2:10) {
  km <- kmeans(normalized_data, centers = k, nstart = 25)
  silhouette_scores[k - 1] <- mean(silhouette(km$cluster, dist(normalized_data, method = "euclidean"))[, 3])
}
plot(2:10, silhouette_scores, type = "b", xlab = "Number of clusters", ylab = "Average silhouette width", main = "Silhouette method")

# Most favored "k"
optimal_k <- 3

# Perform k-means clustering
kmeans_result <- kmeans(normalized_data, centers = optimal_k, nstart = 25)

# Display the cluster centers and cluster assignments
cat("Cluster centers:\n")
print(kmeans_result$centers)
cat("\nCluster assignments:\n")
print(kmeans_result$cluster)

# Calculate the ratio of BSS over TSS, as well as the BSS and WSS
WSS <- sum(kmeans_result$tot.withinss)
TSS <- sum(kmeans_result$totss)
BSS <- TSS - WSS
BSS_ratio <- BSS / TSS

cat("Within-cluster sum of squares (WSS):", WSS, "\n")
cat("Between-cluster sum of squares (BSS):", BSS, "\n")
cat("Ratio of BSS over TSS:", BSS_ratio, "\n")

# Add cluster assignments to the normalized data
normalized_data$cluster <- factor(kmeans_result$cluster)

# Plot the clustered results
fviz_cluster(list(data = normalized_data, cluster = kmeans_result$cluster),
             geom = "point",
             frame.type = "norm",
             main = "Clustered Results",
             xlab = "Variable 1",
             ylab = "Variable 2")

