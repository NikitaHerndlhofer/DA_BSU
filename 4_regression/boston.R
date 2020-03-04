library(h2o)
library(dplyr)
h2o.init()

# Import the boston dataset:
# This dataset looks at features of the boston suburbs and predicts median housing prices
# The original dataset can be found at https://archive.ics.uci.edu/ml/datasets/Housing

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")


# Tasks
# 1) Save boston dataframe to your working directory
# 2) Load boston dataset from your working directory
# Read description of boston dataset (e.g. library(MASS))

str(boston)
summary(boston)

# What types of columns are there in boston?

# Which columns shoud be factors?

# Convert the chas column to a factor (chas = Charles River dummy variable (= 1 if tract bounds river; 0 otherwise))
boston$chas <- as.factor(boston$chas)

# Set the predictor names and the response column name
predictors <- colnames(boston)[1:13]

# Set the response column to "medv", the median value of owner-occupied homes in $1000's
response <- "medv"

# Load boston to h2o
hex_boston <- as.h2o(boston)

# Split into train and validation sets
boston.splits <- h2o.splitFrame(data =  hex_boston, ratios = .8, seed = 42)
train <- boston.splits[[1]]
valid <- boston.splits[[2]]


# Train the model without regularization
boston_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                      validation_frame = valid, lambda = 0, seed = 42, 
                      compute_p_values = TRUE)

h2o.r2(boston_glm, train = TRUE)
# 0.74
h2o.r2(boston_glm, valid = TRUE)
# 0.61


# Print the coefficients table
boston_glm@model$coefficients_table

# How can we assess variable importance based on p value?
# Remove variables with p value > 0.05, build a model and compare perfomance



# Train the model with regularization with default hyperparameters
boston_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                      validation_frame = valid, seed = 42)


boston_glm@parameters$lambda # 0.01339978
boston_glm@parameters$alpha  # 0.5


h2o.r2(boston_glm, train = TRUE)
# 0.74
h2o.r2(boston_glm, valid = TRUE)
# 0.71

# Print the coefficients table
boston_glm@model$coefficients_table

# Conclusion: regularization decrese overfitting

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

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("boston_grid", sort_by = "r2", decreasing = TRUE)
sortedGrid  # alpha = 0 gives the best r2 = 0.71

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model




# Try polynomial features
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

h2o.r2(boston_poly_glm, train = TRUE)
# 0.92
h2o.r2(boston_poly_glm, valid = TRUE)
# 0.79

# Conclusion: adding polynomial features increase r2  



# Train the model for boston_poly with regularization with default hyperparameters
boston_poly_glm <- h2o.glm(x = predictors, y = response, training_frame = train, 
                      validation_frame = valid, seed = 42)

h2o.r2(boston_poly_glm, train = TRUE)
# 0.7
h2o.r2(boston_poly_glm, valid = TRUE)
# 0.67

boston_poly_glm@parameters$lambda
# 1.363193


# grid over `alpha` and 'lambda'
hyper_params <- list(alpha = c(0, .25, .5, .75, 1), lambda = c(1, 0.5, 0.1, 0.01, 0.001))

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: list(strategy = "RandomDiscrete")

# build grid search with previously selected hyperparameters
grid_poly <- h2o.grid(x = predictors, y = response, training_frame = train, validation_frame = valid,
                 algorithm = "glm", grid_id = "boston_grid_poly", hyper_params = hyper_params, seed = 42,
                 search_criteria = list(strategy = "Cartesian"))

summary(grid_poly)

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("boston_grid_poly", sort_by = "r2", decreasing = TRUE)
sortedGrid  # alpha = 0 and lambda = 0.01 gives the best r2 = 0.86 (for valid)

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model

h2o.r2(best_model, train = TRUE)
# 0.88
h2o.r2(best_model, valid = TRUE)
# 0.86

# Conclusion: polynomial features and regularization with alpha = 0 (l2) and lambda = 0.01 increase r2 0.86 on valid
# r2 is about the same on train and on valid, and close to 1 - it is an apropriate fitting
