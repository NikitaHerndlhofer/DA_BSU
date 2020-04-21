# Titanic dataset (classification_titanic_part1.R)

library(ggplot2) 
library(gridExtra)
library(scales) 
library(dplyr)
library(mice) 
library(magrittr)
library(h2o)

localH2O = h2o.init(nthreads=-1) # -1 means use all CPUs on the host (Default)

h2o.removeAll() 

# Load data
train <- read.csv("5_classification/data/train_unsorted.csv", stringsAsFactors=FALSE)
test <- read.csv("5_classification/data/test.csv", stringsAsFactors=FALSE)

# Bind training & test data
full  <- bind_rows(train, test) 

# extractFeatures() 
extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "Survived")
  fea <- data[,features]
  factors <- c("Pclass",
               "Sex",
               "Embarked",
               "Survived")
  fea %<>% mutate_at(factors, as.factor)
  return(fea)
}

# Feature selection
full <- extractFeatures(full)

(y <- as.data.frame(lapply(full[1:7], function(x) sum(is.na(x)|x==""))))

# Show observation with missing Fare
full[is.na(full$Fare),]

# Impute missing Fare
imp_Pclass <- full$Pclass[is.na(full$Fare)]
imp_Embarked <- full$Embarked[is.na(full$Fare)]

full$Fare[is.na(full$Fare)] <- 
  median(full[full$Pclass == imp_Pclass & full$Embarked == imp_Embarked, ]$Fare, na.rm = TRUE)

# Impute missing Embarked
(embark_fare <- full %>% filter(Embarked == ""|is.na(Embarked)))

full[full$Pclass == '1' & ((full$Fare > 79) & (full$Fare < 80)), ]$Embarked

full$Embarked[full$Embarked == ""] <- "C"

# Impute missing Age
set.seed(42) # Set a random seed

sum(is.na(full$Age))  # The result is 263, so all missing values are represented by NA

# Perform mice imputation, excluding Survived variable:
mice_mod <- mice(full[, !names(full) %in% 
                        c('Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model
full$Age <- mice_output$Age


# Split the data back into a train set and a test set
train <- full[1:891,]
new_test <- full[892:1309, 1:7]

write.csv(train, file = '5_classification/data/train.csv', row.names = F)
train <- read.csv("5_classification/data/train.csv")

hex_df <- as.h2o(train)
hex_new <- as.h2o(new_test)

# Splits data into 2 parts
hex_split <- h2o.splitFrame(hex_df, ratios = c(0.8), 
                            destination_frames = c("train", "test"), seed = 42)

nfolds <- 5

myX <- colnames(train[1:7])

# Train & Cross-validate a GLM
my_glm <- h2o.glm(x = myX, y = "Survived",
                  training_frame = hex_split[[1]],
                  model_id = "my_glm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  family = "binomial",
                  seed = 42)

# Check coefficients
my_glm@model$coefficients_table

# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]]) 

pred_test <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]]) 

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8238636

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.8181818

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = myX,
                          y = "Survived",
                          training_frame = hex_split[[1]],
                          model_id = "my_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_rf, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_rf, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9573864

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.7967914

# Variable importance
h2o.varimp(my_rf)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = myX,
                  y = "Survived",
                  training_frame = hex_split[[1]],
                  model_id = "my_gbm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42)


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_gbm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_gbm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9119318

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.8502674

# 1) Make submition of titanic modeling to Kaggle


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

