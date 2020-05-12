  
# 1) Make a Scree Plot for school_result dataset with ggplot()


# 2) Perform Complete Hierarchical Clustering for run_record_sc

# Modify code for single-linkage (below) to complete linkage (hclust with method = "complete")
# Code for single-linkage
# run_dist <- dist(run_record_sc, method = "euclidean")
# run_single <- hclust(run_dist, method = "single")
# memb_single <- cutree(run_single, 5)
# plot(run_single)
# rect.hclust(run_single, k = 5, border = 2:6)


# Code for complete-linkage



# 3) Compare obtained plot with one for single-linkage (see clustering.R) and make a conclusion about clusters obtained via single- and the complete-linkage clustering.



# 4) Compare the membership between the single and the complete linkage clusterings for run_record_sc, using table().



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

# Dunn's index for single-linkage: dunn_single

# Dunn's index for complete-linkage: dunn_complete


# Compare k-means with single-linkage


# Compare k-means with complete-linkage



# 6) Which of the following statements about your clusterings of the countries in run_record_sc are correct? 
# Possible Answers
# 1) The complete-linkage method returned the lowest ratio of minimal intercluster-distance to minimal cluster diameter.
# 2) Based on Dunn's index, the complete-linkage method returned the most compact and separated clusters.
# 3) The single-linkage method returned the highest ratio of minimal intercluster-distance to maximal cluster diameter.
# 4) Based on Dunn's index, the single-linkage method returned the least compact and separated clusters.

# Correct answer is .......



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
crime_data <- read.csv("6_clustering/data/crime_data.csv", stringsAsFactors=FALSE)

# Scale the dataset: crime_data_sc


# Perform k-means clustering: crime_km


# Perform single-linkage hierarchical clustering
## Calculate the distance matrix: dist_matrix

## Calculate the clusters using hclust(): crime_single


## Cut the clusters using cutree: memb_single

# Calculate the Dunn's index for both clusterings: dunn_km, dunn_single


# Print out the results


# Try to print out some of the variables you created and see whether they make sense. 
# How do the techniques compare? What clustering do you offer to your client?
