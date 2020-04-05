install.packages("h2o")
library(h2o)
library(dplyr)
library(tidyverse)
h2o.init()

# Import the boston dataset:
# This dataset looks at features of the boston suburbs and predicts median housing prices
# The original dataset can be found at https://archive.ics.uci.edu/ml/datasets/Housing

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")

View(boston)

# Tasks
# 1) Read description of Boston dataset (hint - library(MASS) contains this dataset)

?MASS::Boston

# 2) Study structure and summary of Boston dataset

MASS::Boston
View(MASS::Boston)
sum(is.na(boston))

dim(boston)

summary(boston)

# 3) What types of columns are there in boston? 

str(boston)

## Which columns shoud be factors?

pairs(boston)


install.packages("corrplot")
library(corrplot)

boston.cor = cor(boston)
View(boston.cor)
corrplot(boston.cor)

# Convert the chas column to a factor (chas = Charles River dummy variable (= 1 if tract bounds river; 0 otherwise))

boston$chas <- as.factor(boston$chas)

View(boston)

## 4) Set the predictors' column names

predictors <- colnames(boston)[1:13]

View(predictors)

## 5) Set the response column to "medv", the median value of owner-occupied homes in $1000's

response <- "medv"

# 6) Load boston to h2o

hex_boston <- as.h2o(boston)

# 7) Split into train and validation sets

boston.split <- h2o.splitFrame(data = hex_boston, ratios = 0.8, seed = 42)
train <- boston.split[[1]]
valid <- boston.split[[2]]

###########################################################################
# 8) Train the model without regularization

glm_boston <- h2o.glm(x = predictors, y = response,
                training_frame = train,
                validation_frame = valid,
                lambda = 0,
                seed = 42)
glm_boston
# 9) Inspect r2

h2o.r2(glm_boston, train = TRUE)
# 0.7430149

h2o.r2(glm_boston, valid = TRUE)
# 0.7110587

# 10) Print the coefficients table

glm_boston@model$coefficients_table

# names coefficients standardized_coefficients
#1  Intercept    31.643625                 22.574344
#2     chas.1     1.235328                  1.235328
#3       crim    -0.070872                 -0.492748
#4         zn     0.030900                  0.733466
#5      indus     0.040504                  0.275426
#6        nox   -15.048549                 -1.734555
#7         rm     4.331346                  3.027048
#8        age    -0.011212                 -0.315577
#9        dis    -1.312637                 -2.757673
#10       rad     0.292907                  2.499092
#11       tax    -0.014002                 -2.318251
#12   ptratio    -0.997957                 -2.114175
#13         b     0.012273                  1.066858
#14     lstat    -0.473709                 -3.365181



###########################################################################
# 11) How can we assess variable importance based on p value?
# Read "Influence of predictors" in "Introduction into ML with R" (available at https://github.com/k-miniukovich/DA_BSU/)

# Remove variables with p value > 0.05, build a model and compare perfomance

high_p = c('chas', 'crim', 'indus', 'age')
predictors_p = predictors[!(predictors %in% high_p)]

glm_boston_p <- h2o.glm(x = predictors_p, y = response,
                      training_frame = train,
                      validation_frame = valid,
                      lambda = 0,
                      seed = 42,
                      compute_p_values = TRUE)

h2o.r2(glm_boston_p, train = TRUE)
# 0.7396167

h2o.r2(glm_boston_p, valid = TRUE)
# 0.684402

###########################################################################
# 12) Train the model for all variables with regularization with default hyperparameters

glm_boston <- h2o.glm(x = predictors, y = response,
                      training_frame = train,
                      validation_frame = valid,
                      seed = 42)


# 13) Check lambda & alfa values

glm_boston@parameters$lambda #0.01339978
glm_boston@parameters$alpha #0.5

# 14) Inspect r2 

h2o.r2(glm_boston, train = TRUE)
# 0.7427546

h2o.r2(glm_boston, valid = TRUE)
# 0.7080632

# 15) Print the coefficients table

glm_boston@model$coefficients_table

#names coefficients standardized_coefficients
#1  Intercept    30.630275                 23.162057
#2     chas.0    -0.582365                 -0.582365
#3     chas.1     0.582365                  0.582365
#4       crim    -0.065563                 -0.455836
#5         zn     0.028376                  0.673566
#6      indus     0.021699                  0.147549
#7        nox   -14.032437                 -1.617434
#8         rm     4.378346                  3.059895
#9        age    -0.011072                 -0.311627
#10       dis    -1.260756                 -2.648677
#11       rad     0.255366                  2.178791
#12       tax    -0.012159                 -2.013079
#13   ptratio    -0.981376                 -2.079048
#14         b     0.012232                  1.063295
#15     lstat    -0.468080                 -3.325194

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

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("boston_grid", sort_by = "r2", decreasing = TRUE)
sortedGrid  # alpha = 0 gives the best r2 = 0.7094759

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
glm_boston_poly <- h2o.glm(x = predictors, y = response, training_frame = train, 
                           validation_frame = valid, lambda = 0, seed = 42)
# Inspect r2 
h2o.r2(glm_boston_poly, train = TRUE)
# 0.9209627
h2o.r2(glm_boston_poly, valid = TRUE)
# 0.7940367

# !!! Conclusion: Does adding polynomial features increase r2?

# Yes

###########################################################################
# 18) Train the model for boston_poly with regularization with default hyperparameters

glm_boston_poly <- h2o.glm(x = predictors, y = response, 
                           training_frame = train, 
                           validation_frame = valid,  
                           seed = 42)

# Inspect r2

h2o.r2(glm_boston_poly, train = TRUE)
# 0.7009533
h2o.r2(glm_boston_poly, valid = TRUE)
# 0.6671481

# Inspect lambda and alpha

glm_boston_poly@parameters$lambda #1.363193
glm_boston_poly@parameters$alpha #0.5

# grid over `alpha` and 'lambda'
hyper_params <- list(alpha = c(0, .25, .5, .75, 1), lambda = c(1, 0.5, 0.1, 0.01, 0.001))

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: list(strategy = "RandomDiscrete")

# build grid search with previously selected hyperparameters

grid_poly <- h2o.grid(x = predictors, y = response, training_frame = train, validation_frame = valid,
                       algorithm = "glm", grid_id = "boston_grid_poly", hyper_params = hyper_params, seed = 42,
                       search_criteria = list(strategy = "Cartesian"))


# Check grid summary

summary(grid_poly)

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("boston_grid_poly", sort_by = "r2", decreasing = TRUE)
sortedGrid  

# What values of alpha and lambda gives the best r2 (for valid)?

# alpha = 1 lambda = 0.5

# Save best model to best_model variable

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model

# Inspect r2

h2o.r2(best_model, train = TRUE)
# 0.8805289
h2o.r2(best_model, valid = TRUE)
# 0.8565232

# Make conclusion about the best obtained model and its' parameters