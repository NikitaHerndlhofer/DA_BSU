install.packages("lubridate")
library("lubridate")
library("ggplot2")
library(h2o)
library(dplyr)
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

View(auto)

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

# visualize dependance between variables

ggplot(data = auto) + 
  geom_point(mapping = aes(x = engine.size, y = price))

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
  geom_point(mapping = aes(x = horsepower, y = price)) +
  geom_smooth(mapping = aes(x = horsepower, y = price))

ggplot(data = auto) + 
  geom_bar(mapping = aes(x = horsepower))

ggplot(auto, aes(x = drive.wheels, y = price)) + 
  geom_boxplot() +
  coord_flip()


# the distribution of price

ggplot(auto, aes(price)) + 
  geom_histogram()

# the distribution of aspiration

ggplot(auto, aes(aspiration)) +
  geom_bar()

auto %>%
  count(aspiration)

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

# Print the coefficients table

glm_reg_auto@model$coefficients_table

