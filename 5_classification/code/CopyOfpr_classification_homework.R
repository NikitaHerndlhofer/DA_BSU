# Titanic dataset (classification_titanic_part1.R)

# 1) Make submition of titanic modeling to Kaggle

# score: 0.75598

# 2) Try XGBoost (default hyperparameters)

my_xgboost <- h2o.xgboost(x = myX, y = "Survived",
                          training_frame = hex_split[[1]],
                          model_id = "my_xgboost",
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)

# Check coefficients
my_xgboost@model$coefficients_table

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

my_ensemble <- h2o.stackedEnsemble(x = myX,
                                   y = "Survived",
                                   training_frame = hex_split[[1]],
                                   model_id = "my_ens",
                                   seed = 42,
                                   base_models = list("my_gbm", "my_rf", "my_glm", "my_xgboost"))


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8948864

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #  0.8502674

# 4) Fill in table_titanic_results.pptx
# 5) Try Grid Search for RF or GBM  and check, if it gives better model than in case with default hyperparameters
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html#grid-search-in-r
 
# GBM hyperparameters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a cartesian grid of GBMs

gbm_grid1 <- h2o.grid("gbm", x = myX,
                      y = "Survived",
                      grid_id = "gbm_grid1",
                      training_frame = hex_split[[1]],
                      validation_frame = hex_split[[2]],
                      ntrees = 100,
                      seed = 42,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

# Grab the top GBM model, chosen by validation AUC
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = hex_split[[1]])
h2o.auc(best_gbm_perf1)
# 0.8805962

# Look at the hyperparameters for the best model
print(best_gbm1@model[["model_summary"]])

library(jsonlite)
library(magrittr)
library(h2o)
library(dplyr)

summary(gbm_grid1)

# OR 
# Makes prediction
pred_train <- as.data.frame(h2o.predict(best_gbm1, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(best_gbm1, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8139205

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #  0.8449198


#Random Grid Search

# GBM hyperparameters (bigger grid than above)
gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria <- list(strategy = "RandomDiscrete", max_models = 36, seed = 42)

# Train and validate a random grid of GBMs
gbm_grid2 <- h2o.grid("gbm", x = myX,
                      y = "Survived",
                      grid_id = "gbm_grid2",
                      training_frame = hex_split[[1]],
                      validation_frame = hex_split[[2]],
                      ntrees = 100,
                      seed = 42,
                      hyper_params = gbm_params2,
                      search_criteria = search_criteria)

gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf2)

# Grab the top GBM model, chosen by validation AUC
best_gbm2 <- h2o.getModel(gbm_gridperf2@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf2 <- h2o.performance(model = best_gbm2,
                                  newdata = hex_split[[1]])
h2o.auc(best_gbm_perf2)
# 0.9392831

# Look at the hyperparameters for the best model
print(best_gbm2@model[["model_summary"]])

# Makes prediction
pred_train <- as.data.frame(h2o.predict(best_gbm2, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(best_gbm2, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8678977

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #   0.8449198

# Extra task on Titanic dataset - classification_titanic_part2.R continuation (not obligatory)

# # Further feature engeneering: 
# 1) check dependences between all other predictors to the response,
# 2) change continuous to discrete variables where reasonable 
# 3) Try to build a model with changed variables and check, if it gives better performance


# Iris dataset

# 1) Make a conclusion about the quality of iris_glm.

hex_iris <- as.h2o(iris)

iris_split <- h2o.splitFrame(hex_iris, ratios = c(0.8), seed = 42)

train <- iris_split[[1]]
test <- iris_split[[2]]

nfolds <- 5
View (iris)
myX <- colnames(train[1:4])

# Train & Cross-validate a GLM
iris_glm <- h2o.glm(x = myX, y = "Species",
                  training_frame = train,
                  model_id = "iris_glm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  family = "multinomial",
                  seed = 42)

# Check coefficients
iris_glm@model$coefficients_table

# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_glm, newdata = iris_split[[1]]))
actual_train <- as.data.frame(iris_split[[1]]) 

pred_test <- as.data.frame(h2o.predict(iris_glm, newdata = iris_split[[2]]))
actual_test <- as.data.frame(iris_split[[2]]) 

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9834711

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9655172


# 2) Build RF and GBM models and assess their performances

iris_rf <- h2o.randomForest(x = myX,
                          y = "Species",
                          training_frame = iris_split[[1]],
                          model_id = "iris_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_rf, newdata = iris_split[[1]]))
actual_train <- as.data.frame(iris_split[[1]])

pred_test <- as.data.frame(h2o.predict(iris_rf, newdata = iris_split[[2]]))
actual_test <- as.data.frame(iris_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9310345

# Variable importance
h2o.varimp(iris_rf)

# Train & Cross-validate a GBM
iris_gbm <- h2o.gbm(x = myX,
                  y = "Species",
                  training_frame = iris_split[[1]],
                  model_id = "iris_gbm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_gbm, newdata = iris_split[[1]]))
actual_train <- as.data.frame(iris_split[[1]])

pred_test <- as.data.frame(h2o.predict(iris_gbm, newdata = iris_split[[2]]))
actual_test <- as.data.frame(iris_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #  0.9310345


# 3) Chose the best of 3 models for iris, justify your chose
# лучше iris_glm, так как метрика на тесте лучше

# Boston dataset

# 1) Build RF and GBM models (at least default) and assess their performances

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")

boston$chas <- as.factor(boston$chas)

hex_boston <- as.h2o(boston)

boston_split <- h2o.splitFrame(hex_boston, ratios = c(0.8), seed = 42)

train <- boston_split[[1]]
valid <- boston_split[[2]]

nfolds <- 5

myX <- colnames(train[1:13])

boston_rf <- h2o.randomForest(x = myX,
                            y = "medv",
                            training_frame = boston_split[[1]],
                            validation_frame = valid,
                            model_id = "boston_rf",
                                                seed = 42)


h2o.r2(boston_rf, train = TRUE)
# 0.8730297
h2o.r2(boston_rf, valid = TRUE)
# 0.8746264
 

# Variable importance
h2o.varimp(boston_rf)

# Train & Cross-validate a GBM
boston_gbm <- h2o.gbm(x = myX,
                    y = "medv",
                    training_frame = train,
                    validation_frame = valid,
                    model_id = "boston_gbm",
                    nfolds = nfolds,
                    keep_cross_validation_predictions = TRUE, # need for ensemble
                    seed = 42)

h2o.r2(boston_gbm, train = TRUE)
# 0.9660849
h2o.r2(boston_gbm, valid = TRUE)
# 0.8481494

# 2) Compare obtained results with ones in table_boston_results.pptx

