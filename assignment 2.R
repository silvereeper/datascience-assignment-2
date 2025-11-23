# Step 1: Initialization

student_data <- data.frame(
  Student = paste0("S", 1:8),
  Math = c(80, 85, 88, 40, 42, 50, 75, 78),
  English = c(78, 82, 90, 45, 48, 52, 70, 74)
)


data_for_clustering <- student_data[, 2:3]
rownames(data_for_clustering) <- student_data$Student

print("--- Prepared Data ---")
print(data_for_clustering)

# Step 2: Calculate Proximity Matrix
distance_matrix <- dist(data_for_clustering, method = "euclidean")

print("--- Euclidean Distance Matrix (Proximity Matrix) ---")
print(distance_matrix)

# Steps 3 & 4:
hac_model <- hclust(distance_matrix, method = "average")

print("--- Hierarchical Clustering Model Summary ---")
print(hac_model)

# Step 5: Repetition and Stopping Condition
plot(hac_model, 
     main = "Agglomerative Hierarchical Clustering (Average Linkage)",
     xlab = "Student", 
     ylab = "Distance (Height of Merge)",
     hang = -1)

k_clusters <- 3
cluster_assignment <- cutree(hac_model, k = k_clusters)


rect.hclust(hac_model, k = k_clusters, border = 2:4)

print(paste("--- Final Cluster Assignment (k =", k_clusters, ") ---"))

final_data_with_clusters <- cbind(data_for_clustering, Cluster = cluster_assignment)
print(final_data_with_clusters)