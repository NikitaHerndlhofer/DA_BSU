# Adult income dataset 

library(readr)
library(ggplot2)
library(dplyr)
library(h2o)
library(xgboost)
library(magrittr)

adult <- read_csv("Project/data/adult.csv")
View(adult)
str(adult)

# Data Cleaning

adult[["education.num"]]=NULL

adult[["fnlwgt"]]=NULL

adult[["capital.gain"]]=NULL

adult[["capital.loss"]]=NULL  

View(adult)


# Mapping some continuous attributes to nominal attributes

## Age Combining (classification according WHO)

table(adult$age)

adult$age <- cut(adult$age, c(17,44,59,74,90), labels = c("Young", "Middle", "Senior", "Old"), ordered=TRUE)

adult$age <- as.character(adult$age)

barplot(table(adult$age))

##  Working schedule

table(adult$hours.per.week)

adult$hours.per.week <- cut(adult$hours.per.week, c(0,25,40,60,168), labels = c("Part-time", "Full-time", "Over-time", "Too-much"), ordered=TRUE)

adult$hours.per.week <- as.character(adult$hours.per.week)

barplot(table(adult$hours.per.week))

# Reducing number of levels of some nominal attributes

## Work Class Combining

table(adult$workclass)

adult$workclass <- as.character(adult$workclass)

adult$workclass[adult$workclass == "Without-pay" | 
                  adult$workclass == "Never-worked"] <- "Unemployed"

adult$workclass[adult$workclass == "State-gov" |
                  adult$workclass == "Local-gov"] <- "SL-gov"

adult$workclass[adult$workclass == "Self-emp-inc" |
                  adult$workclass == "Self-emp-not-inc"] <- "Self-employed"

table(adult$workclass)

## Dealing with Missing Data

table(adult$workclass)
adult[adult == "?"] <- NA
table(adult$workclass)

barplot(table(adult$workclass))

## Marital Status Combining

table(adult$marital.status)

adult$marital.status <- as.character(adult$marital.status)

adult$marital.status[adult$marital.status == "Married-AF-spouse" |
                       adult$marital.status == "Married-civ-spouse" |
                       adult$marital.status == "Married-spouse-absent"] <- "Married"

adult$marital.status[adult$marital.status == "Divorced" |
                       adult$marital.status == "Never-married"|
                       adult$marital.status == "Separated" |
                       adult$marital.status == "Widowed"] <- "Not-Married"

table(adult$marital.status)

## Education Combining

table(adult$education)

adult$education <- as.character(adult$education)

adult$education[adult$education == "1st-4th" |
                  adult$education == "5th-6th" ] <- "Elementary School"

adult$education[adult$education == "7th-8th" ] <- "Middle School"

adult$education[adult$education == "9th" |
                  adult$education == "10th" |
                  adult$education == "11th" |
                  adult$education == "12th" ] <- "High school"

adult$education[adult$education == "Assoc-acdm"|
                  adult$education == "Assoc-voc"]<-"Associate Degree"

adult$education[adult$education == "Some-college" |
                  adult$education == "Bachelors" |
                  adult$education == "Masters" |
                  adult$education == "Doctorate" ] <- "Higher education"

barplot(table(adult$education))

## Others 

# Dealing with Missing Data

install.packages(Amelia)
library(Amelia) # program for missing data
missmap(adult, y.at = 1, y.labels = "", col = c("red", "black"), legend = FALSE)
adult <- na.omit(adult)
missmap(adult, y.at = 1, y.label = "", legend = FALSE, col = c("red", "black"))

# extractFeatures() 
extractFeatures <- function(data) {
  features <- c("age",
                "workclass",
                "sex",
                "marital.status",
                "occupation",
                "relationship",
                "race",
                "native.country",
                "hours.per.week",
                "education",
                "income")
  fea <- data[,features]
  factors <- c("income",
               "workclass",
               "marital.status",
               "sex",
               "occupation",
               "relationship",
               "race",
               "native.country",
               "education", 
               "age",
               "hours.per.week")
  fea %<>% mutate_at(factors, as.factor)
  return(fea)
}
adult <- extractFeatures(adult)


# Exploratory Data Analysis

## Income by age

ggplot(adult, aes(x = age, color = income, fill = income)) +
  geom_density(alpha = 0.8) +
  labs(x = "Age", y = "Density", 
       subtitle = "Density plot")

ggsave("Project/graphs/age.png")

## Income based on working class

ggplot(adult, aes(x = workclass, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Working Class", y = "Proportion")

ggsave("Project/graphs/working_class.png")

## Dependence of income from education

ggplot(adult, aes(x = education, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Education Level", y = "Proportion")

ggsave("Project/graphs/education.png")

## Dependence of income from marital status and sex

ggplot(adult, aes(x=income, y=1, fill=marital.status)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~sex) +
  labs( y = "Proportion")

ggsave("Project/graphs/marital_status&sex.png")

## Dependence of income from race

ggplot(adult, aes(x = race, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Race", y = "Proportion")

ggsave("Project/graphs/race.png")

## Income by work schedule

ggplot(adult, aes(hours.per.week)) + 
  geom_histogram(aes(fill = income), stat = "count") +
  labs(y = "Proportion", x = "Work schedule")

ggsave("Project/graphs/schedule.png")

## Dependence of income from occupation

ggplot(adult, aes(x = occupation, fill = income, color = income)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(x = "Occupation", y = "Proportion")

ggsave("Project/graphs/occupation.png")

#  Loading to h2o

localH2O = h2o.init(nthreads=-1)

h2o.removeAll() 

adult <- as.data.frame(adult)

hex_adult <- as.h2o(adult)

## Splits data into 2 parts

adult_split <- h2o.splitFrame(hex_adult, ratios = c(0.8), 
                              destination_frames = c("train", "test"), seed = 42)

train <- adult_split[[1]]
valid <- adult_split[[2]]

myX <- colnames(train[1:10])
nfolds <- 5
set.seed(42)

# Gbm
my_gbm <- h2o.gbm(x = myX, y = "income",
                  training_frame = train,
                  model_id = "my_gbm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42) 
# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_gbm, newdata = adult_split[[1]]))
actual_train <- as.data.frame(adult_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_gbm, newdata = adult_split[[2]]))
actual_test <- as.data.frame(adult_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$income, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.826038

tbl_test <- table(actual_test$income, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.8222866

# Glm
my_xgb <- h2o.xgboost(x = myX, y = "income",
                      training_frame = train,
                      model_id = "my_xgb",
                      nfolds = nfolds,
                      keep_cross_validation_predictions = TRUE, # need for ensemble
                      seed = 42) 
# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_xgb, newdata = adult_split[[1]]))
actual_train <- as.data.frame(adult_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_xgb, newdata = adult_split[[2]]))
actual_test <- as.data.frame(adult_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$income, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.8273315

tbl_test <- table(actual_test$income, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.821605

# RandomForest
my_rf <- h2o.randomForest(x = myX,
                          y = "income",
                          training_frame =  train,
                          model_id = "my_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_rf, newdata = adult_split[[1]]))
actual_train <- as.data.frame(adult_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_rf, newdata = adult_split[[2]]))
actual_test <- as.data.frame(adult_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$income, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.8406009

tbl_test <- table(actual_test$income, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.8079741

# Stacked Ensemble with RF, GBM and XGBoost
new_ensemble <- h2o.stackedEnsemble(x = myX,
                                    y = "income",
                                    training_frame = train,
                                    model_id = "new_ens",
                                    seed = 42,
                                    base_models = list("my_gbm", "my_rf", "my_xgb"))

# Makes prediction
pred_train <- as.data.frame(h2o.predict(new_ensemble, newdata = adult_split[[1]]))
actual_train <- as.data.frame(adult_split[[1]])

pred_test <- as.data.frame(h2o.predict(new_ensemble, newdata = adult_split[[2]]))
actual_test <- as.data.frame(adult_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$income, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.832005

tbl_test <- table(actual_test$income, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.8245016

# Cartesian grid of GBMs

# Set grid hyperparameters 
hyper_params <- list(ntrees = c(50, 75, 100),
                     max_depth = c(3,5,7),
                     learn_rate = c(0.05, 0.1, 0.2),
                     sample_rate = c(0.8, 1.0),
                     col_sample_rate = c(0.7, 1))

gbm_grid1 <- h2o.grid("gbm",
                      x = myX,
                      y = "income",
                      training_frame = train,
                      validation_frame = valid,
                      grid_id = "gbm_grid1",
                      hyper_params = hyper_params,
                      seed = 42)
summary(gbm_grid1)

# Get the grid results, sorted by validation accuracy
sortedGrid <- h2o.getGrid("gbm_grid1", sort_by = "accuracy", decreasing = TRUE)
sortedGrid  

# Save best model to best_model variable
best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model@allparameters

# Makes prediction
pred_train <- as.data.frame(h2o.predict(best_model, newdata = adult_split[[1]]))
pred_test <- as.data.frame(h2o.predict(best_model, newdata = adult_split[[2]]))


# Calculates accuracies
tbl_train <- table(actual_train$income, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) 
# 0.81715
tbl_test <- table(actual_test$income, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) 
# 0.8178565 

best_model <- new_ensemble
pred <- h2o.predict(best_model, valid)
res <- as.data.frame(pred$predict)

View(res)
solution <- data.frame(actual_test, income_pred = res$predict)
write.csv(solution, file = 'Project/Solution.csv', row.names = F)
