library(ggplot2) # visualization
library(gridExtra) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(magrittr)
library(h2o)

localH2O = h2o.init(nthreads=-1) # -1 means use all CPUs on the host (Default)

h2o.removeAll() # Clean slate - just in case the cluster was already running


# Set working directory
setwd("D:/R projects/ds-courses/BSU/classification")

# Load data
train <- read.csv("data/train.csv", stringsAsFactors=FALSE)
test <- read.csv("data/test.csv", stringsAsFactors=FALSE)

# Know data structure and some statistics
str(train)
str(test)

summary(train)
summary(test)


# Bind training & test data
full  <- bind_rows(train, test) 


# Columns'PassengerId', 'Name', 'Ticket', 'Cabin' won't be included into the simple model. Think why.

# Explain, what extractFeatures() function does
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
  fea %<>% mutate_at(factors, list(as.factor))
  return(fea)
}

# Feature selection
full <- extractFeatures(full)

View(full)
# Why NA in Survived column? Should we impute these NA?


# Show number of missing values for features

# Read help on lapply() and explain a row of the code below
# What external brackets do?
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


# The same as two rows of code above but automated. Try to understand
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# full$Embarked[full$Embarked == ""] <- 
#   getmode(full[full$Pclass == '1' & ((full$Fare > 79) & (full$Fare < 80)), ]$Embarked)



# Impute missing Age
set.seed(42) # Set a random seed

sum(is.na(full$Age))  # The result is 263, so all missing values are represented by NA

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


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.8196023

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.8466667


# Example of code to get performance metrics
# perf <- h2o.performance(my_glm, train = TRUE)  # valid for validation, xval for cv
# perf@metrics$cm$table
# h2o.auc(my_glm, train = TRUE) # valid for validation, xval for cv
# my_glm@model$training_metrics@metrics$cm$table


# Example of code to save and load models
# mdir <- sprintf("%s/models", getwd())
# glmmodel.path <- h2o.saveModel(my_glm, mdir, force = TRUE)
# glm <- h2o.loadModel(glmmodel.path)




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
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9559659

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.96





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
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.90625

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9133333





# Train a stacked ensemble using the GLM, GBM and RF above
my_ensemble <- h2o.stackedEnsemble(x = myX,
                                y = "Survived",
                                training_frame = hex_split[[1]],
                                model_id = "my_ens",
                                seed = 42,
                                base_models = list("my_gbm", "my_rf", "my_glm"))


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9




# Automl http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
# sort_metric specifies the metric used to sort the Leaderboard by at the end of an AutoML run. 
# By default sort_metric = AUTO. This defaults to AUC for binary classification, 
# mean_per_class_error for multinomial classification, and deviance for regression.

aml <- h2o.automl(x = myX, y = "Survived",
                  training_frame = hex_split[[1]],
                  # leaderboard_frame = hex_split[[2]],
                  max_runtime_secs = 300,
                  seed = 42)

lb <- aml@leaderboard
print(lb, n = nrow(lb))

aml@leader

perf_aml_test <- h2o.performance(aml@leader, newdata = hex_split[[2]])
aml_auc_test <- h2o.auc(perf_aml_test)
print(sprintf("AML Test AUC:  %s",aml_auc_test)) 


# Makes prediction
pred_train <- as.data.frame(h2o.predict(aml@leader, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(aml@leader, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$Survived, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.85

tbl_test <- table(actual_test$Survived, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.84


# Conclusion: the best model is ...


# Predit with the best model for hex_new ans prepare file for submition to Kaggle

best_model <- my_rf
pred <- h2o.predict(best_model, hex_new)
res<-as.data.frame(pred$predict)

solution <- data.frame(PassengerID = test$PassengerId, Survived = res$predict)

# Write the solution to file
write.csv(solution, file = 'aml_Solution.csv', row.names = F)


# Tasks
# 1) Read about Grid Search http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html#grid-search-in-r
# 2) Try Grid Search for RF and GBM algorithms

