# Example of a case when log transformation make relations more close to linear and as a result 
# usage of glm more effective


library(ggplot2)
library(h2o)

# setwd("D:/R projects/ds-courses/BSU/regression")

# load world_bank_train.csv and world_bank_test.csv from data dir
world_bank_train <- read.csv("4_regression/data/world_bank_train.csv")
world_bank_test <- read.csv("4_regression/data/world_bank_test.csv", sep=';')

# world_bank_train (_test):
# cgdp - GDP per Capita, urb_pop - Percentage Urban Population

summary(world_bank_train)
str(world_bank_train)

summary(world_bank_test)
str(world_bank_test)

# Plot urb_pop as function of cgdp
g1 <- ggplot() +  geom_point(data = world_bank_train, aes(cgdp, urb_pop))
g1
g2 <- ggplot() +  geom_point(data = world_bank_test, aes(cgdp, urb_pop))
g2

train <- as.h2o(world_bank_train)
test <- as.h2o(world_bank_test)

# Train a glm model
glm_world_bank <- h2o.glm(x = "cgdp", y = "urb_pop",
                   training_frame = train,
                   validation_frame = test,
                   model_id = "glm_world_bank",
                   seed = 42)


h2o.r2(glm_world_bank, train = TRUE)
# 0.38

h2o.r2(glm_world_bank, valid = TRUE)
# 0.37

# Conclusion: r2<<1, the model performance is weak

intercept <- glm_world_bank@model$coefficients[[1]]
slope <- glm_world_bank@model$coefficients[[2]]

g1 + geom_abline(intercept = intercept, slope = slope, colour = "red")




# Change cgdp to log(cgdp)
world_bank_train$logcgdp <- log(world_bank_train$cgdp)
world_bank_test$logcgdp <- log(world_bank_test$cgdp)

# Plot urb_pop as function of cgdp
g1 <- ggplot() +  geom_point(data = world_bank_train, aes(logcgdp, urb_pop))
g1
g2 <- ggplot() +  geom_point(data = world_bank_test, aes(logcgdp, urb_pop))
g2

train <- as.h2o(world_bank_train)
test <- as.h2o(world_bank_test)

# Train a glm model
glm_world_bank <- h2o.glm(x = "logcgdp", y = "urb_pop",
                          training_frame = train,
                          validation_frame = test,
                          model_id = "glm_world_bank",
                          seed = 42)


h2o.r2(glm_world_bank, train = TRUE)
# 0.58

h2o.r2(glm_world_bank, valid = TRUE)
# 0.53

# Conclusion: using log improved model performance 

intercept <- glm_world_bank@model$coefficients[[1]]
slope <- glm_world_bank@model$coefficients[[2]]

g1 + geom_abline(intercept = intercept, slope = slope, colour = "red")

