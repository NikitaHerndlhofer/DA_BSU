###Clustering
library(ggplot2)
library(clValid) # for dunn() function

# Set working directory
setwd("D:/R projects/ds-courses/BSU/clustering")

# While clusters are made without the use of true labels, if you happen to have them, it is simply interesting to see 
# how well the clusters you made correspond to these true labels.

# It's up to you now to cluster the instances in seeds and compare the resulting clusters with seeds_type. 

# Load seeds.csv and seeds_type.csv from dir data
seeds <- read.csv("data/seeds.csv", stringsAsFactors=FALSE)
seeds_type <- read.csv("data/seeds_type.csv", stringsAsFactors=FALSE)


# Know data structure and some statistics 
str(seeds)
summary(seeds)

str(seeds_type)
summary


# Set random seed. 
set.seed(42)

# Do k-means clustering with three clusters, repeat 20 times: seeds_km
seeds_km <- kmeans(seeds, centers = 3, nstart = 20)

# Print out seeds_km
seeds_km

# Calculate WSS/TSS and compare with 0.2
seeds_km$tot.withinss/seeds_km$totss

# Compare clusters with actual seed types. Set k-means clusters as rows
table(seeds_km$cluster, seeds_type$type)

# Plot the length as function of width. Color by cluster
g1 <- ggplot() +  geom_point(data = seeds, aes(x = width, y = length), colour = seeds_km$cluster) + ylab("Length") + xlab("Width")
g1

#  If you have a look at the table that got generated, you clearly see three groups with 60 elements or more. Overall, you can say that your formed clusters adequately represent adequately the different types of seeds.
# These larger groups represent the correspondence between the clusters and the actual types. E.g. Cluster 1 corresponds to seed_type 3.





###The Influence of Starting Centroids
# If you call kmeans() without specifying your centroids, R will randomly assign them for you. In this exercise, you will see the influence of these starting centroids yourself using the seeds dataset.

# To compare the clusters of two cluster models, you can again use table(). If every row and every column has one value, the resulting clusters completely overlap. 
# If this is not the case, some objects are placed in different clusters.


# Apply kmeans to seeds twice: seeds_km_1 and seeds_km_2
seeds_km_1 <- kmeans(seeds, 5, nstart = 1)
seeds_km_2 <- kmeans(seeds, 5, nstart = 1)

# Print the ratio of the WSS, which is stored in the tot.withinssof of seeds_km_1 and seeds_km_2. 
# Put the first one in the numerator.
seeds_km_1$tot.withinss / seeds_km_2$tot.withinss

# Compare the resulting clusters. Put the clusters of seeds_km_1 in the rows.
table(seeds_km_1$cluster, seeds_km_2$cluster)


# Conclusion
# As you can see, some clusters remained the same, others have changed. For example, cluster 5 from seeds_km_1 completely contains cluster 1 from seeds_km_2 (33 objects). 
# Cluster 4 from seeds_km_1 is split, 18 objects were put in seeds_km_2's fourth cluster and 41 in its fifth cluster. 
# For consistent and decent results, you should set nstart > 1 or determine a prior estimation of your centroids.





###Making A Scree Plot

# Load school_result.csv from dir data
school_result <- read.csv("data/school_result.csv", stringsAsFactors=FALSE)

# Explore the structure of the data
str(school_result)

# Dataset school_result containing school level data recording reading and arithmetic scores for each school's 4th and 6th graders. (Source: cluster.datasets package).
# We're wondering if it's possible to define distinct groups of students based on their scores and if so how many groups should we consider?

# Your job is to cluster the schools based on their scores with k-means, for different values of k. 
# On each run, you'll record the ratio of the within cluster sum of squares to the total sum of squares. 
# The scree plot will tell you which k is optimal!


# Initialize a vector of length 7, ratio_ss, that contains all zeros. You can use rep().
ratio_ss <- rep(0, 7)

# Finish the for-loop
for (k in 1:7) {
  
  # Apply k-means to school_result: school_km
  school_km <- kmeans(school_result, k, nstart = 20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
  
}

# Plot the ratio_ss. Set the type argument in plot() to "b", connecting the points. Also set the xlab argument to "k".
plot(ratio_ss, type = "b", xlab = "k")

# How to make the same plot with ggplot()????????????????


# You want to choose k such that your clusters are compact and well separated. However, the ratio_ss keeps decreasing as k increases. 
# Hence, if you were to minimize this ratio as function of k, you'd end up with a cluster for every school, which is a meaningless result. 
# You should choose k such that when increasing it, the impact on ratio_ss is not significant. 
# The elbow in the scree plot will help you identify this turning point.

# Which of the following values of k will provide a meaningful clustering with overall compact and well separated clusters? 
#  1 or 2
# 3 or 4
# 5 or 6





###Standardized vs Non-Standardized Clustering (1)
# Is standardization important for clustering algorithms?

# First cluster the countries based on their unstandardized records and calculate Dunn's index.
# Then do the same for standardized records and compare the quality of clustering.

# Load run_record.csv from dir data
run_record <- read.csv("data/run_record.csv", stringsAsFactors=FALSE)

# Explore your data with str() and summary()
str(run_record)
summary(run_record)

# Cluster run_record using k-means: run_km. 5 clusters, repeat 20 times
run_km <- kmeans(run_record, 5, nstart = 20)

# Plot the 100m as function of the marathon. Color using clusters
g2 <- ggplot() +  geom_point(data = run_record, aes(x = marathon, y = X100m), colour = run_km$cluster) + ylab("100") + xlab("marathon") 
g2

# Calculate Dunn's index: dunn_km. Print it.
dunn_km <- dunn(clusters = run_km$cluster, Data = run_record)
print(dunn_km)

# Conclusion
# As you can see in the plot, the unstandarized clusters are completely dominated by the marathon records; you can even separate every cluster only based on the marathon records! 
# Moreover Dunn's index seems to be quite low. 
# Compare your results with the standardized version in the next exercise.


###Standardized vs Non-Standardized Clustering (2)

# Standardize run_record, transform to a dataframe: run_record_sc
run_record_sc <- as.data.frame(scale(run_record))

# Cluster run_record_sc using k-means: run_km_sc. 5 groups, let R start over 20 times
run_km_sc <- kmeans(run_record_sc, 5, nstart = 20)

# Plot records on 100m as function of the marathon. Color using the clusters in run_km_sc
g3 <- ggplot() +  geom_point(data = run_record_sc, aes(x = marathon, y = X100m), colour = run_km_sc$cluster) + ylab("100") + xlab("marathon") 
g3


# Compare the resulting clusters in a nice table
table(run_km$cluster, run_km_sc$cluster)

# Calculate Dunn's index: dunn_km_sc. Print it.
dunn_km_sc <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)
dunn_km_sc

# Conclusion
# The plot now shows the influence of the 100m records on the resulting clusters! 
# Dunn's index is clear about it, the standardized clusters are more compact or/and better separated!




###Single Hierarchical Clustering


# Calculate the Euclidean distance matrix of run_record_sc using dist(). 
# Assign it to run_dist. dist() uses the Euclidean method by default.
run_dist <- dist(run_record_sc)

# Use the run_dist matrix to cluster your data hierarchically, based on single-linkage. 
# Use hclust() with two arguments. Assign it to run_single.
run_single <- hclust(run_dist, method = "single")

# Cut the tree using cutree() at 5 clusters. Assign the result to memb_single.
memb_single <- cutree(run_single, k = 5)

# Make a dendrogram of run_single using plot(). 
# If you pass a hierarchical clustering object to plot(), it will draw the dendrogram of this clustering.
plot(run_single)

# Draw boxes around the 5 clusters using rect.hclust(). Set the border argument to 2:6, for different colors.
rect.hclust(run_single, k = 5, border = 2:6)

# Conclusion
# It appears the two islands Samoa and Cook's Islands, who are not known for their sports performances, have both been placed in their own groups. 
# Maybe, we're dealing with some chaining issues? Let's try a different linkage method in the next exercise!


###Complete Hierarchical Clustering
# run_record_sc is pre-loaded

# Modify code for single-linkage (below) to complete linkage (hclust with method = "complete")
# Code for single-linkage
# run_dist <- dist(run_record_sc, method = "euclidean")
# run_single <- hclust(run_dist, method = "single")
# memb_single <- cutree(run_single, 5)
# plot(run_single)
# rect.hclust(run_single, k = 5, border = 2:6)


# Code for complete-linkage
run_complete <- hclust(run_dist, method = "complete")
memb_complete <- cutree(run_complete, k = 5)
plot(run_complete)
rect.hclust(run_complete, k = 5, border = 2:6)

# Compare the membership between the single and the complete linkage clusterings, using table().
table(memb_single, memb_complete)

# Conclusion
# Compare the two plots. The five clusters differ significantly from the single-linkage clusters. 
# That one big cluster you had before, is now split up into 4 medium sized clusters. 
# Have a look at the table you generated as well!






###Hierarchical vs k-means
# So you've clustered the countries based on their Olympic run performances using three different methods: k-means clustering, hierarchical clustering with single linkage and hierarchical clustering with complete linkage. 
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

# Conclusion
# The table shows that the clusters obtained from the complete linkage method are similar to those of k-means. 
# Now it's up to you to compare the values of Dunn's index and decide which method returns the best separated and most compact clusters.

# Which of the following statements about your clusterings of the countries in run_record_sc is correct? 
# Possible Answers
# 1) The complete-linkage method returned the lowest ratio of minimal intercluster-distance to minimal cluster diameter.
# 2) Based on Dunn's index, the complete-linkage method returned the most compact and separated clusters.
# 3) The single-linkage method returned the highest ratio of minimal intercluster-distance to maximal cluster diameter.
# 4) Based on Dunn's index, the single-linkage method returned the least compact and separated clusters.

# Correct answer is 3




###Clustering US States Based on Criminal Activity
# You've seen that different clustering methods can return entirely different clusters, each with their own interpretation and uses. 
# It's time to put your skills, both the programming and the interpretation, to the test!

# Your client has provided you with a dataset, crime_data (crime_data.csv in dir data), containing info on the crimes committed in each of the 50 US states and the percentage of urban population (Source: Edureka). He'd like you to group the states in 4 clusters. 
# He didn't specify which similarity to use, but the euclidean distance seems acceptable, don't you agree?

# You decide to try out two techniques: k-means and single-linkage hierarchical clustering. 
# You then want to compare the results by calculating the Dunn's indices to make a conclusion. 
# Which clustering will you deliver to your client?


# Load the data
crime_data <- read.csv("data/crime_data.csv", stringsAsFactors=FALSE)

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

# Conclusion
# Try to print out some of the variables you created and see whether they make sense. 
# How do the techniques compare? What clustering do you offer to your client?