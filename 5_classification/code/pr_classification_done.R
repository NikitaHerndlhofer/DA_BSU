library(ggplot2) 
library(gridExtra) 
library(scales) 
library(dplyr) 
library(mice) 
library(magrittr)
library(h2o)

# Titanic dataset (classification_titanic_part1.R)

localH2O = h2o.init(nthreads=-1)

h2o.removeAll() 


# Load data
train <- read.csv("/cloud/project/5_classification/data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("/cloud/project/5_classification/data/test.csv", stringsAsFactors=FALSE)

# Bind training & test data
full  <- bind_rows(train, test) 

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
# [1] C C S S S C C C C
# Levels:  C Q S
full$Embarked[full$Embarked == ""] <- "C"

# Impute missing Age
set.seed(42)

sum(is.na(full$Age))

# Perform mice imputation, excluding Survived variable:
mice_mod <- mice(full[, !names(full) %in% 
                        c('Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)


# Plot age distributions
g1 <- ggplot() +  geom_histogram(data = full, aes(Age), colour = 'green') + xlab('Age: Original Data')

g2 <- ggplot() + geom_histogram(data = mice_output, aes(Age), colour = 'blue') + xlab('Age: MICE Output')

grid.arrange(g1, g2, nrow = 1)


# Replace Age variable from the mice model
full$Age <- mice_output$Age

# Split the data back into a train set and a test set
train <- full[1:891,]
new_test <- full[892:1309, 1:7]
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
                          keep_cross_validation_predictions = TRUE, 
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
                  keep_cross_validation_predictions = TRUE,
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



# 2) Try XGBoost (default hyperparameters)
my_xgboost <- h2o.xgboost(x = myX,
                      y = "Survived",
                      training_frame = hex_split[[1]],
                      model_id = "my_xgboost",
                      nfolds = nfolds,
                      keep_cross_validation_predictions = TRUE,
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
new_ensemble <- h2o.stackedEnsemble(x = myX,
                                    y = "Survived",
                                    training_frame = hex_split[[1]],
                                    model_id = "new_ensemble",
                                    seed = 42,
                                    base_models = list("my_gbm", "my_rf", "my_glm", "my_xgboost"))

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

# Grid summary
summary(gbm_grid)

# Get the grid results, sorted by validation accuracy
sortedGrid <- h2o.getGrid("gbm_grid", sort_by = "accuracy", decreasing = TRUE)
sortedGrid  

# Save best model to best_model variable
best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model@allparameters

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
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.9834711

tbl_test <- table(actual_test$Species, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.9655172

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

h2o.r2(gbm_boston, train = TRUE) # 0.9660849

h2o.r2(gbm_boston, valid = TRUE) # 0.8481494

rf_boston <- h2o.randomForest(x = predictors, y = response,
                              training_frame = train,
                              validation_frame = valid,
                              seed = 42)

h2o.r2(rf_boston, train = TRUE) # 0.8730297

h2o.r2(rf_boston, valid = TRUE) # 0.8746264
