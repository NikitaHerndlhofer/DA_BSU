# Multiclass classification

library(h2o)

localH2O = h2o.init(nthreads=-1) # -1 means use all CPUs on the host (Default)

h2o.removeAll() # Clean slate - just in case the cluster was already running


# Set working directory
# setwd("D:/R projects/ds-courses/BSU/classification")

# Analyze Iris dataset
str(iris)

# Formulate mclass classification problem (task) for Iris dataset. 
# How many classes are there in this problem?

# Check if classes balanced
iris %>% group_by(Species) %>% summarise(n())

# Species    `n()`
# <fct>      <int>
# 1 setosa        50
# 2 versicolor    50
# 3 virginica     50

# Conclusion: classes are balanced

sum(is.na(iris)) # no NA values

hex_df <- as.h2o(iris)

# Splits data into 2 parts
hex_split <- h2o.splitFrame(hex_df, ratios = c(0.8), 
                            destination_frames = c("train", "test"), seed = 42)

nfolds <- 5

myX <- colnames(iris[1:4])

# Train & Cross-validate a GLM
iris_glm <- h2o.glm(x = myX, y = "Species",
                  training_frame = hex_split[[1]],
                  model_id = "iris_glm",
                  nfolds = nfolds,
                  family = "multinomial",
                  seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_glm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(iris_glm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.98

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.97


# Calculates mean per class error (h2o recommended performance metrics for mclass classification)
perf_train <- h2o.performance(iris_glm, train = TRUE)  
h2o.mean_per_class_error(perf_train)

perf_val <- h2o.performance(iris_glm, xval = TRUE)  
h2o.mean_per_class_error(perf_val)

perf_test <- h2o.performance(iris_glm, hex_split[[2]])  
h2o.mean_per_class_error(perf_test)













