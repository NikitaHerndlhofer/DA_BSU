# 1) Make a Scree Plot for school_result dataset with ggplot()

install.packages("ICtest")
library(ICtest)

n <- 25
X1 <- cbind(rnorm(school_result$reading.4),rnorm(school_result$arithmetic.4), rnorm(school_result$reading.6), rnorm(school_result$arithmetic.6))
X2 <- cbind(pnorm(school_result$reading.4),pnorm(school_result$arithmetic.4), pnorm(school_result$reading.6), pnorm(school_result$arithmetic.6))
Test1 <- PCAasymp(X1, k = 4)
ggscreeplot(Test1) # 
Test2 <- PCAasymp(X2, k = 4)
ggscreeplot(Test2) # 

School <- PCAasymp(school_result, k = 4)
ggscreeplot(School)


# 2) Perform Complete Hierarchical Clustering for run_record_sc

# Modify code for single-linkage (below) to complete linkage (hclust with method = "complete")
# Code for single-linkage
run_dist <- dist(run_record_sc, method = "euclidean")
run_single <- hclust(run_dist, method = "single")
memb_single <- cutree(run_single, 5)
plot(run_single)
rect.hclust(run_single, k = 5, border = 2:6)


# Code for complete-linkage

run_complete <- hclust(run_dist, method = "complete")
memb_complete <- cutree(run_complete, k = 5)
plot(run_complete)
rect.hclust(run_complete, k = 5, border = 2:6)

# 3) Compare obtained plot with one for single-linkage (see clustering.R) and make a conclusion about clusters obtained via single- and the complete-linkage clustering.

# In single-linkage: the similarity of their most similar members, clusters with the smallest minimum pairwise distance
# In complete-linkage: the similarity of their most dissimilar members, clusters with the smallest maximum pairwise distance

# 4) Compare the membership between the single and the complete linkage clusterings for run_record_sc, using table().

table(memb_single, memb_complete)


# 5) Hierarchical vs k-means
# So you've clustered the countries based on their Olympic run performances using three different methods: 
# k-means clustering, hierarchical clustering with single linkage and hierarchical clustering with complete linkage. 
# You can ask yourself: which method returns the best separated and the most compact clusters?

# Let's use Dunn's index. Remember, it returns the ratio between the minimum intercluster distance to the maximum intracluster diameter. 
# The dunn() function in R, requires the argument clusters, indicating the cluster partitioning, the Data and a method to determine the distance. 
# In this case, that's "euclidean", which is the default.

# Your job is to calculate Dunn's index for all three clusterings and compare the clusters to each other. 
# Set random seed. Don't remove this line.


# Dunn's index for k-means: dunn_km
dunn_km <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)

# Dunn's index for single-linkage: dunn_single
dunn_single <- dunn(clusters = memb_single, Data = run_record_sc)

# Dunn's index for complete-linkage: dunn_complete
dunn_complete <- dunn(clusters = memb_complete, Data = run_record_sc)


# Compare k-means with single-linkage
table(run_km_sc$cluster, memb_single)

# Compare k-means with complete-linkage
table(run_km_sc$cluster, memb_complete)


# 6) Which of the following statements about your clusterings of the countries in run_record_sc are correct? 
# Possible Answers
# 1) The complete-linkage method returned the lowest ratio of minimal intercluster-distance to minimal cluster diameter.
# 2) Based on Dunn's index, the complete-linkage method returned the most compact and separated clusters.
# 3) The single-linkage method returned the highest ratio of minimal intercluster-distance to maximal cluster diameter.
# 4) Based on Dunn's index, the single-linkage method returned the least compact and separated clusters.

# Correct answer is 3


# 7) Clustering US States Based on Criminal Activity
# You've seen that different clustering methods can return entirely different clusters, each with their own interpretation and uses. 
# It's time to put your skills, both the programming and the interpretation, to the test!

# Your client has provided you with a dataset, crime_data (crime_data.csv in dir data), 
# containing info on the crimes committed in each of the 50 US states and the percentage of urban population (Source: Edureka). 
# He'd like you to group the states in 4 clusters. 
# He didn't specify which similarity to use, but the euclidean distance seems acceptable, don't you agree?

# You decide to try out two techniques: k-means and single-linkage hierarchical clustering. 
# You then want to compare the results by calculating the Dunn's indices to make a conclusion. 
# Which clustering will you deliver to your client?


# Load the data
crime_data <- read.csv("/cloud/project/6_clustering/data/crime_data.csv", stringsAsFactors=FALSE)

# Scale the dataset: crime_data_sc
crime_data_sc <- scale(crime_data)

# Perform k-means clustering: crime_km
crime_km <- kmeans(crime_data_sc, 4, nstart = 20)

# Perform single-linkage hierarchical clustering
## Calculate the distance matrix: dist_matrix
dist_matrix <- dist(crime_data_sc)

## Calculate the clusters using hclust(): crime_single
crime_single <- hclust(dist_matrix, method = "single")

## Cut the clusters using cutree: memb_single
memb_single <- cutree(crime_single, 4)

# Calculate the Dunn's index for both clusterings: dunn_km, dunn_single
dunn_km <- dunn(clusters = crime_km$cluster, Data = crime_data_sc)
dunn_single <- dunn(clusters = memb_single, Data = crime_data_sc)


# Print out the results
print(dunn_km)
print(dunn_single)

# Try to print out some of the variables you created and see whether they make sense. 
# How do the techniques compare? What clustering do you offer to your client?