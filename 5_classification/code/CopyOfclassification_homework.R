# 1) Make submition of titanic modeling to Kaggle

# score: 0.75598

# 2) Try XGBoost (default hyperparameters)
my_xgb <- h2o.xgboost(x = myX,
                      y = "Survived",
                      training_frame = hex_split[[1]],
                      model_id = "my_xgb",
                      nfolds = nfolds,
                      keep_cross_validation_predictions = TRUE,
                      seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_xgb, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_xgb, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9588068

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.802139

# 3) Try Stacked Ensemble with GLM, RF, GBM and XGBoost
new_ensemble <- h2o.stackedEnsemble(x = myX,
                                    y = "Survived",
                                    training_frame = hex_split[[1]],
                                    model_id = "new_ens",
                                    seed = 42,
                                    base_models = list("my_gbm", "my_rf", "my_glm", "my_xgb"))

# Makes prediction
pred_train <- as.data.frame(h2o.predict(new_ensemble, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(new_ensemble, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8948864

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) #  0.8502674

# 4) Fill in table_titanic_results.pptx
# 5) Try Grid Search for RF or GBM  and check, if it gives better model than in case with default hyperparameters
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html#grid-search-in-r

library(h2o)
library(jsonlite)
h2o.init()

gbm_params <- list(ntrees = c(100, 200, 300),
                   max_depth = c(3, 5, 9),
)

hf_train <- as.h2o(train)
hf_test <- as.h2o(test)


gbm_grid <- h2o.grid("gbm", x = myX, y = "Survived",
                     grid_id = "gbm_grid",
                     training_frame = hf_train,
                     seed = 42,
                     hyper_params = gbm_params,
                     search_criteria = list(strategy = "Cartesian"))

?h2o.grid

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "r2",
                            decreasing = TRUE)
print(gbm_gridperf)

best_gbm <- h2o.getModel(gbm_gridperf@model_ids[[1]])

print(best_gbm)
h2o.r2(best_gbm, train = TRUE) 
# 0.8843772


# Iris dataset

hex_df <- as.h2o(iris)

# Splits data into 2 parts
hex_split <- h2o.splitFrame(hex_df, ratios = c(0.8), 
                            destination_frames = c("train", "test"), seed = 42)

nfolds <- 5

myX <- colnames(iris[1:4])

# Train & Cross-validate a GLM
iris_glm <- h2o.glm(x = myX, 
                    y = "Species",
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
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9834711

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9655172

# 1) Make a conclusion about the quality of iris_glm.

# 2) Build RF and GBM models and assess their performances

# Train & Cross-validate a RF
iris_rf <- h2o.randomForest(x = myX,
                            y = "Species",
                            training_frame = hex_split[[1]],
                            model_id = "iris_rf",
                            ntrees = 50,
                            nfolds = nfolds,
                            seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_rf, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(iris_rf, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9310345

# Train & Cross-validate a GBM
iris_gbm <- h2o.gbm(x = myX,
                    y = "Species",
                    training_frame = hex_split[[1]],
                    model_id = "iris_gbm",
                    nfolds = nfolds,
                    seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(iris_gbm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(iris_gbm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Species, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9310345
# 3) Chose the best of 3 models for iris, justify your chose


# Boston dataset

boston <- read.csv("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/BostonHousing.csv")

boston$chas <- as.factor(boston$chas)

predictors <- colnames(boston)[1:13]
response <- "medv"

hex_boston <- as.h2o(boston)

boston.split <- h2o.splitFrame(data = hex_boston, ratios = 0.8, seed = 42)
train <- boston.split[[1]]
valid <- boston.split[[2]]

# 1) Build RF and GBM models (at least default) and assess their performances 

gbm_boston <- h2o.gbm(x = predictors, y = response,
                      training_frame = train,
                      validation_frame = valid,
                      seed = 42)

h2o.r2(gbm_boston, train = TRUE)
# 0.9660849

h2o.r2(gbm_boston, valid = TRUE)
# 0.8481494

rf_boston <- h2o.randomForest(x = predictors, y = response,
                              training_frame = train,
                              validation_frame = valid,
                              seed = 42)

h2o.r2(rf_boston, train = TRUE)
# 0.8730297

h2o.r2(rf_boston, valid = TRUE)
# 0.8746264

# 2) Compare obtained results with ones in table_boston_results.pptx
