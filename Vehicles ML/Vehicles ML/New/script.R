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
