install.packages("lubridate")
install.packages("pracma")
library("lubridate")
library(pracma)
library("ggplot2") # visualization
library(gridExtra) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(magrittr)
library(h2o)
h2o.init()

# https://www.geeksforgeeks.org/analyzing-selling-price-of-used-cars-using-python/
# https://rstudio-pubs-static.s3.amazonaws.com/345340_a8ed48c5ed094a94a4d60242a8c94a24.html

# Import the automobile dataset:
# This dataset looks at various characteristics of an auto, insurance risk rating, and predicts prices of cars
# The original dataset can be found at https://archive.ics.uci.edu/ml/datasets/automobile

auto <- read.csv("/cloud/project/Project_automobile/data/automobile_data.csv")

View(auto)

# For each variable see meaning, type and values. 
str (auto)
summary(auto)

auto$symboling<-as.factor(auto$symboling)
auto$normalized.losses<-as.numeric(levels(auto$normalized.losses))[auto$normalized.losses]
auto$bore<-as.numeric(levels(auto$bore))[auto$bore]
auto$stroke<-as.numeric(levels(auto$stroke))[auto$stroke]
auto$horsepower<-as.numeric(levels(auto$horsepower))[auto$horsepower]
auto$peak.rpm<-as.numeric(levels(auto$peak.rpm))[auto$peak.rpm]
auto$price<-as.numeric(levels(auto$price))[auto$price]

# Working with missing value 

(y <- as.data.frame(lapply(auto, function(x) sum(is.na(x)))))

# auto[auto =="?"]<-0

auto<-auto[complete.cases(auto$price),]
auto$normalized.losses[is.na(auto$normalized.losses)]=mean(auto$normalized.losses,na.rm=TRUE)
auto$horsepower[is.na(auto$horsepower)]=mean(auto$horsepower,na.rm=TRUE)
auto$peak.rpm[is.na(auto$peak.rpm)]=mean(auto$peak.rpm,na.rm=TRUE)
auto$bore[is.na(auto$bore)]=mean(auto$bore,na.rm=TRUE)
auto$stroke[is.na(auto$stroke)]=mean(auto$stroke,na.rm=TRUE)

# mean num.of.doors won't be correct
# table(auto$num.of.doors)
# auto$num.of.doors[auto$num.of.doors ==''] <-'four'

View(auto)

# visualization

# Normalizing values
auto$length = auto$length / max(auto$length)
auto$width = auto$width / max(auto$width) 
auto$height = auto$height / max(auto$height)

# binning- grouping values 
bins = linspace(min(auto$price), max(auto$price), 4)  
group_names = c("Low", "Medium", "High") 
auto$price.binned <- cut(auto$price, bins,  
                              labels = group_names,  
                              include_lowest = True) 

(y <- as.data.frame(lapply(auto, function(x) sum(is.na(x)))))
auto<-auto[complete.cases(auto$price.binned),]

View(auto)

# the distribution of price.binned

ggplot(auto, aes(price.binned)) + 
  geom_bar(fill = "LawnGreen", col="MediumVioletRed")
ggsave("img/price_binnen.png")

auto %>%
  count(price.binned)

ggplot(data = auto) + 
  geom_point(mapping = aes(x = engine.size, y = price, color = engine.type))
ggsave("img/engine.size vs price.png")

# pivot method 
data_pivot = data_grp.pivot(index = 'drive-wheels', 
                            columns = 'body-style') 
data_pivot

# heatmap
plot.pcolor(data_pivot, cmap ='RdBu') 
plt.colorbar() 
plt.show() 

ggplot(data = auto) + 
  geom_point(mapping = aes(x = city.mpg, y = price, color = fuel.system))

ggplot(data = auto) + 
  geom_point(mapping = aes(x = wheel.base, y = price, alpha = compression.ratio))

ggplot(data = auto) + 
  geom_point(mapping = aes(x = horsepower, y = price, shape = body.style))

ggplot(data = auto) + 
  geom_point(mapping = aes(x = horsepower, y = price)) + 
  facet_wrap(~ body.style)

ggplot(data = auto) + 
  geom_point(mapping = aes(x = engine.size, y = price)) +
  geom_smooth(mapping = aes(x = engine.size, y = price))

ggplot(data = auto) + 
  geom_bar(mapping = aes(x = horsepower))

ggplot(auto, aes(x = drive.wheels, y = price)) + 
  geom_boxplot() +
  coord_flip()


# the distribution of price

ggplot(auto, aes(price)) + 
  geom_histogram()



# regression model

# model<-lm(price~.,data=auto)
# summary(model)

predictors <- colnames(auto)[1:25]

response <- "price"

hex_auto <- as.h2o(auto)

# Split into train and validation sets

auto.splits <- h2o.splitFrame(data =  hex_auto, ratios = .8, seed = 42)
train <- auto.splits[[1]]
valid <- auto.splits[[2]]

# Train the model without regularization

auto_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                    remove_collinear_columns = TRUE,
                    validation_frame = valid, lambda = 0, seed = 42,
                    compute_p_values = TRUE)
auto_glm

# Inspect r2
h2o.r2(auto_glm, train = TRUE)
# 0.9721814
h2o.r2(auto_glm, valid = TRUE)
# 0.9064784

# Print the coefficients table

auto_glm@model$coefficients_table

# Remove variables with p value > 0.05, build a model and compare perfomance

high_p = c('stroke', 'compression.ratio', 'city.mpg', 'highway.mpg')
predictors_p = predictors[!(predictors %in% high_p)]

auto_glm_p <- h2o.glm(x = predictors_p, y = response, training_frame = train, 
                        validation_frame = valid, 
                        remove_collinear_columns = TRUE, lambda = 0, seed = 42,
                        compute_p_values = TRUE)

h2o.r2(auto_glm_p, train = TRUE)
# 0.9704175
h2o.r2(auto_glm_p, valid = TRUE)
# 0.8858182

auto_glm_p@model$coefficients_table

# Train the model for all variables with regularization with default hyperparameters

glm_reg_auto <- h2o.glm(x = predictors, y = response, training_frame = train, 
                          validation_frame = valid, 
                      remove_collinear_columns = TRUE,  seed = 42)
glm_reg_auto

# Check lambda & alfa values

glm_reg_auto@parameters$lambda # 1243.597
glm_reg_auto@parameters$alpha # 0.5

# Inspect r2 

h2o.r2(glm_reg_auto, train = TRUE)
# 0.8316786
h2o.r2(glm_reg_auto, valid = TRUE)
# 0.8897386

# ОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКА
# Print the coefficients table 

glm_reg_auto@model$coefficients_table

# select the values for `alpha` to grid over
hyper_params <- list( alpha = c(0, .25, .5, .75) )

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: {'strategy': "RandomDiscrete"}

# build grid search with previously selected hyperparameters
grid <- h2o.grid(x = predictors, y = response, training_frame = train, validation_frame = valid,
                 algorithm = "glm", grid_id = "auto_grid", hyper_params = hyper_params, seed = 42,
                 search_criteria = list(strategy = "Cartesian"))

summary(grid)

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("auto_grid", sort_by = "r2", decreasing = TRUE)
sortedGrid  

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
best_model

# ОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКА

# 17) Polynomial features of degree 2 for all predictors except symboling, make, fuel.type, aspiration, num.of.doors, 
# body.style, drive.wheels, engine.location, engine.type, num.of.cylinders, fuel.system (factors)

# detach("package:MASS", unload = TRUE)

dat <- select(auto, -c(symboling, make, fuel.type, aspiration, num.of.doors, body.style, drive.wheels,
                         engine.location, engine.type, num.of.cylinders, fuel.system, price))
View(dat)

auto_poly <- as.data.frame(do.call(poly, c(lapply(1:length(dat), function(x) dat[,x]), degree=2, raw=T)))
auto_poly$make<- auto$make
auto_poly$symboling <- auto$symboling
auto_poly$fuel.type <- auto$fuel.type
auto_poly$aspiration <- auto$aspiration
auto_poly$num.of.doors <- auto$num.of.doors
auto_poly$body.style <- auto$body.style
auto_poly$drive.wheels <- auto$drive.wheels
auto_poly$engine.location <- auto$engine.location
auto_poly$engine.type <- auto$engine.type
auto_poly$num.of.cylinders <- auto$num.of.cylinders
auto_poly$fuel.system <- auto$fuel.system
auto_poly$price <- auto$price

# ОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКАОШИБКА

# classification

# extractFeatures <- function(data) {
#  features <- c("normalized.losses",
#               "fuel.type",
#                "aspiration",
#                "engine.location",
#                "wheel.base",
#                "length",
#                "width",
#                "height",
#                "curb.weight",
#                "engine.size",
#                "bore",
#                "stroke", 
#                "compression.ratio",
#                "horsepower",
#                "peak.rpm",
#                "city.mpg",
#                "highway.mpg",
#                "price")
#  fea <- data[,features]
#  factors <- c("fuel.type",
#               "aspiration",
#               "engine.location")
#  fea %<>% mutate_at(factors, list(as.factor))
#  return(fea)
# }

# Feature selection
# auto <- extractFeatures(auto)



# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = myX,
                          y = "price",
                          training_frame = train,
                          validation_frame = valid,
                          model_id = "my_rf",
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE, # need for ensemble
                          seed = 42)


# Inspect r2
h2o.r2(my_rf, train = TRUE)
# 0.9181694
h2o.r2(my_rf, valid = TRUE)
# 0.9447359

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = myX,
                  y = "price",
                  training_frame = train,
                  model_id = "my_gbm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  seed = 42)

# Inspect r2
h2o.r2(my_gbm, train = TRUE)
# 0.9181694
h2o.r2(my_gbm, valid = TRUE)
# 0.9447359

# XGBoost (default hyperparameters)

my_xgboost <- h2o.xgboost(x = myX, y = "price",
                          training_frame = train,
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
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9473684


# Stacked Ensemble with GLM, RF, GBM and XGBoost

my_ensemble <- h2o.stackedEnsemble(x = myX,
                                   y = "price.binned",
                                   training_frame = train,
                                   model_id = "my_ensemble",
                                   seed = 42,
                                   base_models = list("my_gbm", "my_rf", "my_glm", "my_xgboost"))


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0


hex_auto <- as.h2o(auto)

# Splits data into 2 parts
hex_split <- h2o.splitFrame(hex_auto, ratios = c(0.8), 
                            destination_frames = c("train", "test"), seed = 42)

nfolds <- 5

myX <- colnames(train[1:25])
# Train & Cross-validate a GLM
my_glm <- h2o.glm(x = myX, y = "price.binned",
                  training_frame = hex_split[[1]],
                  model_id = "my_glm",
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE, # need for ensemble
                  family = "multinomial",
                  seed = 42)

# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_glm, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0.9012346

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.8947368

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = myX,
                          y = "price.binned",
                          training_frame = train,
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
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9473684

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = myX,
                  y = "price.binned",
                  training_frame = train,
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
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9473684

# XGBoost (default hyperparameters)

my_xgboost <- h2o.xgboost(x = myX, y = "price",
                          training_frame = train,
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
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 1

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0.9473684


# Stacked Ensemble with GLM, RF, GBM and XGBoost

my_ensemble <- h2o.stackedEnsemble(x = myX,
                                    y = "price.binned",
                                    training_frame = train,
                                    model_id = "my_ensemble",
                                    seed = 42,
                                    base_models = list("my_gbm", "my_rf", "my_glm", "my_xgboost"))


# Makes prediction
pred_train <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[1]]))
actual_train <- as.data.frame(hex_split[[1]])

pred_test <- as.data.frame(h2o.predict(my_ensemble, newdata = hex_split[[2]]))
actual_test <- as.data.frame(hex_split[[2]])

# Calculates accuracies
tbl_train <- table(actual_train$price.binned, pred_train$predict)
(accuracy_train <- sum(diag(tbl_train)) / sum(tbl_train)) # 0

tbl_test <- table(actual_test$price.binned, pred_test$predict)
(accuracy_test <- sum(diag(tbl_test)) / sum(tbl_test)) # 0






