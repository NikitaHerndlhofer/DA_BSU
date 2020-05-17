# Titanic dataset (classification_titanic_part1.R)

getwd() # "/cloud/project"
setwd("/cloud/project/5_classification")

# 1) Make submition of titanic modeling to Kaggle

# score: 0.75598
# place in leaderboard: 155....

# 2) Try XGBoost (default hyperparameters)

my_xgboost <- h2o.xgboost(x = myX, y = "Survived",
                  training_frame = hex_split[[1]],
                  model_id = "my_xgboost",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_xgboost, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_xgboost, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9588068

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.802139


# 3) Try Stacked Ensemble with GLM, RF, GBM and XGBoost

my_ensemble1 <- h2o.stackedEnsemble(x = myX,
                                   y = "Survived",
                                   training_frame = hex_split[[1]],
                                   model_id = "my_ensemble",
                                   seed = 42,
                                   base_models = list("my_gbm", "my_rf", "my_glm", "my_xgboost"))


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_ensemble1, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_ensemble1, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8948864

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.8502674

# 4) Fill in table_titanic_results.pptx

# done

# 5) Try Grid Search for RF or GBM  and check, if it gives better model than in case with default hyperparameters
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html#grid-search-in-r
 
library(jsonlite)
library(magrittr)
library(h2o)
library(dplyr)

# set grid hyperparameters 
hyper_params <- list(ntrees = c(50, 75, 100),
                     max_depth = c(3,5,7),
                     learn_rate = c(0.05, 0.1, 0.2),
                     sample_rate = c(0.8, 1.0),
                     col_sample_rate = c(0.7, 1))

# Default: ntrees=50, max_depth=5, learn_rate=0.1, col_sample_rate=1

# Example below uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: list(strategy = "RandomDiscrete")

# Train and validate a cartesian grid of GBMs

gbm_grid <- h2o.grid("gbm",
                      x = myX,
                      y = "Survived",
                      training_frame = hex_split[[1]],
                      validation_frame = hex_split[[2]],
                      grid_id = "gbm_grid",
                      hyper_params = hyper_params,
                      seed = 42)

# Check grid summary
summary(gbm_grid)

# Get the grid results, sorted by validation accuracy
sortedGrid <- h2o.getGrid("gbm_grid", sort_by = "accuracy", decreasing = TRUE)
sortedGrid  

# Save best model to best_model variable
best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model@allparameters

# Makes prediction
pred_train <- as.data.frame(h2o.predict(best_model, newdata = hex_split[[1]]))
pred_test <- as.data.frame(h2o.predict(best_model, newdata = hex_split[[2]]))


# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.85
tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #  0.87


# Extra task on Titanic dataset - classification_titanic_part2.R continuation (not obligatory)

# # Further feature engeneering: 
# 1) check dependences between all other predictors to the response,
# 2) change continuous to discrete variables where reasonable 
# 3) Try to build a model with changed variables and check, if it gives better performance


# Iris dataset

# 1) Make a conclusion about the quality of iris_glm.

hex_iris <- as.h2o(iris)

iris.splits <- h2o.splitFrame(data =  hex_iris, ratios = .8, seed = 42)
train <- iris.splits[[1]]
test <- iris.splits[[2]]

nfolds <- 5

myX <- colnames(train[1:4])

iris_glm <- h2o.glm(x = myX, y = "Species",
                  training_frame = train,
                  model_id = "iris_glm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  family = "multinomial",
                  seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_glm, newdata = train))
actual_train <- as.data.frame(train)

pred_test <- as.data.frame(h2o.predict(iris_glm, newdata = test))
actual_test <- as.data.frame(test)

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9834711

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9655172

# 2) Build RF and GBM models and assess their performances

iris_rf <- h2o.randomForest(x = myX,
                          y = "Species",
                          training_frame = train,
                          model_id = "iris_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_rf, newdata = train))
actual_train <- as.data.frame(train)

pred_test <- as.data.frame(h2o.predict(iris_rf, newdata = test))
actual_test <- as.data.frame(test)

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9310345

iris_gbm <- h2o.gbm(x = myX,
                  y = "Species",
                  training_frame = train,
                  model_id = "iris_gbm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_gbm, newdata = train))
actual_train <- as.data.frame(train)

pred_test <- as.data.frame(h2o.predict(iris_gbm, newdata = test))
actual_test <- as.data.frame(test)

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9310345

# 3) Chose the best of 3 models for iris, justify your chose
# лучше iris_glm, так как метрика на тесте лучше

# Boston dataset

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")

boston$chas <- as.factor(boston$chas)

hex_boston <- as.h2o(boston)

boston.splits <- h2o.splitFrame(data =  hex_boston, ratios = .8, seed = 42)
train <- boston.splits[[1]]
valid <- boston.splits[[2]]

predictors <- colnames(boston)[1:13]

# 1) Build RF and GBM models (at least default) and assess their performances 

boston_rf <- h2o.randomForest(x = predictors,
                            y = "medv",
                            training_frame = train,
                            validation_frame = valid, 
                            model_id = "boston_rf", 
                            seed = 42)

h2o.r2(boston_rf, train = TRUE)
# 0.8730297
h2o.r2(boston_rf, valid = TRUE)
# 0.8746264

boston_gbm <- h2o.gbm(x = predictors,
                              y = "medv",
                              training_frame = train,
                              validation_frame = valid, 
                              model_id = "boston_rf", 
                              seed = 42)

h2o.r2(boston_gbm, train = TRUE)
# 0.9660849
h2o.r2(boston_gbm, valid = TRUE)
# 0.8481494

# 2) Compare obtained results with ones in table_boston_results.pptx

# these ones are better then results in table_boston
