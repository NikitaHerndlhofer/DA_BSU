library(h2o)
library(dplyr)
h2o.init()

# Import the boston dataset:
# This dataset looks at features of the boston suburbs and predicts median housing prices
# The original dataset can be found at https://archive.ics.uci.edu/ml/datasets/Housing

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")


# Tasks
# 1) Read description of Boston dataset (hint - library(MASS) contains this dataset)


# 2) Study structure and summary of Boston dataset


# 3) What types of columns are there in boston?

# Which columns shoud be factors?

# Convert the chas column to a factor (chas = Charles River dummy variable (= 1 if tract bounds river; 0 otherwise))


# 4) Set the predictors' column names


# 5) Set the response column to "medv", the median value of owner-occupied homes in $1000's


# 6) Load boston to h2o


# 7) Split into train and validation sets



###########################################################################
# 8) Train the model without regularization


# 9) Inspect r2


# 10) Print the coefficients table



###########################################################################
# 11) How can we assess variable importance based on p value?
# Read "Influence of predictors" in "Introduction into ML with R" (available at https://github.com/k-miniukovich/DA_BSU/)




###########################################################################
# 12) Train the model for all variables with regularization with default hyperparameters


# 13) Check lambda & alfa values



# 14) Inspect r2 


# 15) Print the coefficients table


# !!!!! Conclusion: Does regularization affects overfitting?


###########################################################################
# 16) Study the example and understand how grid search work
# grid over `alpha`
# select the values for `alpha` to grid over
hyper_params <- list( alpha = c(0, .25, .5, .75) )

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: {'strategy': "RandomDiscrete"}

# build grid search with previously selected hyperparameters
grid <- h2o.grid(x = predictors, y = response, training_frame = train, validation_frame = valid,
                 algorithm = "glm", grid_id = "boston_grid", hyper_params = hyper_params, seed = 42,
                 search_criteria = list(strategy = "Cartesian"))

summary(grid)

# Sort the grid models by r2
sortedGrid <- h2o.getGrid("boston_grid", sort_by = "r2", decreasing = TRUE)
sortedGrid  

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model



###########################################################################
# 17) Try polynomial features of degree 2 for all predictors except chas (factor)
dat <- select(boston, -c(chas, medv))
boston_poly <- as.data.frame(do.call(poly, c(lapply(1:length(dat), function(x) dat[,x]), degree=2, raw=T)))
boston_poly$chas <- boston$chas
boston_poly$medv <- boston$medv

predictors <- colnames(boston_poly)[1:91]

# Load boston to h2o
hex_boston_poly <- as.h2o(boston_poly)

# Split into train and validation sets
boston_poly.splits <- h2o.splitFrame(data =  hex_boston_poly, ratios = .8, seed = 42,)
train <- boston_poly.splits[[1]]
valid <- boston_poly.splits[[2]]


# Train the model for boston_poly without regularization
boston_poly_glm <- h2o.glm(x = predictors, y = response, training_frame = train, 
                           validation_frame = valid, lambda = 0, seed = 42)
# Inspect r2 
h2o.r2(boston_poly_glm, train = TRUE)

h2o.r2(boston_poly_glm, valid = TRUE)


# !!! Conclusion: Does adding polynomial features increase r2?


###########################################################################
# 18) Train the model for boston_poly with regularization with default hyperparameters


# Inspect r2


# Inspect lambda and alpha



# grid over `alpha` and 'lambda'
hyper_params <- list(alpha = c(0, .25, .5, .75, 1), lambda = c(1, 0.5, 0.1, 0.01, 0.001))

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: list(strategy = "RandomDiscrete")

# build grid search with previously selected hyperparameters


# Check grid summary


# Sort the grid models by r2
sortedGrid <- h2o.getGrid("boston_grid_poly", sort_by = "r2", decreasing = TRUE)
sortedGrid  

# What values of alpha and lambda gives the best r2 (for valid)?


# Save best model to best_model variable


# Inspect r2


# Make conclusion about the best obtained model and its' parameters