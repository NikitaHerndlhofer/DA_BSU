library(ggplot2)
library(h2o)

# Simple Linear Regression-----------------------------------------------------

# We will start with the most familiar linear regression, a straight-line fit to data. 
# y=ax+b, where a is commonly known as the slope, and b - as the intercept.

set.seed(42)

# Generate synthetic example
x <- 10 * runif(100)
y <- 2 * x - 5 + rnorm(100)
sim <- data.frame(x, y)

# Data Frame Functions
dim(sim)
str(sim)
summary(sim)

# What runif() and rnorm() do?

# Visualize sim
g <- ggplot(sim) + geom_point(aes(x, y))
g

localH2O = h2o.init(nthreads=-1) # -1 means use all CPUs on the host (Default)

# Open link http://localhost:54321/flow/index.html in browser to go to h2o flow

# Load sim to h2o
hex_sim <- as.h2o(sim)

# Train a glm model
glm_sim <- h2o.glm(x = "x", y = "y",
                  training_frame = hex_sim,
                  model_id = "glm_sim",
                  seed = 42)

glm_sim
# Intercept -4.903550, slope 1.960503
# R^2 :  0.9721786

xnew <- 10 * runif(1000)
hex_xnew <- as.h2o(xnew) 
ynew <- predict(glm_sim, hex_xnew)

predict <- as.data.frame(ynew)
colnames(predict) <- "y"
predict$x <- xnew
  
ggplot() +
  geom_point(data = sim, aes(x, y)) +
  geom_line(data = predict, aes(x, y), colour = "red")
  


# For GLM in h2o it doesn't matter if data is sorted. But for some other linear model algorithms it does. 
# In this case data should be shuffled before splitting. You can think of this like shuffling 
# a brand new deck of playing cards before dealing hands.
# Example of code for shuffling

n <- nrow(sim)
shuffled_sim <- sim[sample(n), ]


# Model validation-------------------------------------------------------------

hex_split <- h2o.splitFrame(hex_sim, ratios = c(0.8), 
                            destination_frames = c("train", "valid"), seed = 42)

# Train a glm model with validation
glm_v_sim <- h2o.glm(x = "x", y = "y",
                   training_frame = hex_split[[1]],
                   validation_frame = hex_split[[2]],
                   model_id = "glm_v_sim",
                   seed = 42)
glm_v_sim

# Analyze H2ORegressionMetrics on training and cross-validation data  

# Train a glm model with cross-validation
glm_cv3_sim <- h2o.glm(x = "x", y = "y",
                   training_frame = hex_sim,
                   nfolds = 3,
                   model_id = "glm_cv3_sim",
                   seed = 42)

glm_cv3_sim


# Use glm to model non-linear relations ---------------------------------------

# Generate example of non-linear relations between x and y
set.seed(42) # Why do we use seed?
x1 <-  10 * runif(50)
y1 <-  sin(x1) + 0.1*rnorm(50)
sim1 <- data.frame(x1, y1)
summary(sim1)

g1 <- ggplot() +  geom_point(data = sim1, aes(x1, y1))
g1

# Make polynomial features
# for one feature
sim1_poly <- as.data.frame(poly(sim1[,1], degree = 7, raw = T))
poly_names <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7')
colnames(sim1_poly) <- poly_names
sim1_poly$y <- sim1$y1
View(sim1_poly)

# for many features
# dat <- data.frame(1:3, 2:4)
# dat_poly <- as.data.frame(do.call(poly, c(lapply(1:length(dat), function(x) dat[,x]), degree=2, raw=T)))
# View(dat_poly)


# Train a glm model with cross-validation for sim1_poly
hex_poly <- as.h2o(sim1_poly)
glm_cv3_sim1poly <- h2o.glm(x = poly_names, y = "y",
                       training_frame = hex_poly,
                       nfolds = 3,
                       model_id = "glm_cv3_sim1poly",
                       lambda = 0, # no regularization
                       seed = 42)

# Print the model
glm_cv3_sim1poly

# Print the coefficients
glm_cv3_sim1poly@model$coefficients

# Predicy on new data
xnew <-  11 * runif(100)
xnew_poly <- as.data.frame(poly(xnew, degree = 7, raw = T))
colnames(xnew_poly) <- poly_names

hex_xnew_poly <- as.h2o(xnew_poly)

predict <- as.data.frame(predict(glm_cv3_sim1poly, hex_xnew_poly))
colnames(predict) <- "y"
predict$x <- xnew

g2 <- g1 + geom_line(data = predict, aes(x, y), colour = "red")
g2

# Notice that the extrapolation outside the range of the data is clearly bad.
# This is the downside to approximating a function with a polynomial. 
# But this is a very real problem with every model: the model can never tell you 
# if the behaviour is true when you start extrapolating outside the range of the data that you have seen.

# Try modeling for sim1 poly with degree 3, 5, 9. Compare the results.




# Regularization ---------------------------------------------------------------
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html#

# Penalties can be introduced to the model building process to: 
# 1) to avoid overfitting, 
# 2) to reduce variance of the prediction error, 
# 3) to handle correlated predictors.

# Two most common penalized models are Ridge regression and LASSO.
# The elastic net combines both penalties using both the alpha and lambda options (i.e., values greater than 0 for both).

# LASSO represents the ℓ1 penalty and penalizes the sum of the absolute coefficents.
# LASSO leads to a sparse solution when the tuning parameter is sufficiently large. 
# As the tuning parameter value λ is increased, all coefficients are set to zero. 
# Because reducing parameters to zero removes them from the model, LASSO is a good selection tool.

# Ridge regression (ℓ2) penalizes the sum of squares of coefficients. 
# It provides greater numerical stability and is easier and faster to compute than LASSO. 
# It keeps all the predictors in the model and shrinks them proportionally. 
# Ridge regression reduces coefficient values simultaneously as the penalty is increased without setting any of them to zero.

# Elastic net

# The alpha parameter controls the distribution between the ℓ1 (LASSO) and ℓ2 (ridge regression) penalties. 
# A value of 1.0 for alpha represents LASSO, and an alpha value of 0.0 produces ridge reguression.

# The lambda parameter controls the amount of regularization applied. If lambda is 0.0, no regularization is applied, 
# and the alpha parameter is ignored. The default value for lambda is calculated by H2O using a heuristic based on the training data. 
# If you allow H2O to calculate the value for lambda, you can see the chosen value in the model output.
# lambda_search example http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/lambda_search.html

# Train a glm model with regularization for sim1_poly
glm_reg_sim1poly <- h2o.glm(x = poly_names, y = "y",
                            training_frame = hex_poly,
                            nfolds = 3,
                            model_id = "glm_reg_sim1poly",
                            lambda = 0.0001,
                            seed = 42)

# Print the model
glm_reg_sim1poly

# Print the coefficients
glm_reg_sim1poly@model$coefficients


predict_reg <- as.data.frame(predict(glm_reg_sim1poly, hex_xnew_poly))
colnames(predict_reg) <- "y"
predict_reg$x <- xnew

g1 <- g1 + geom_line(data = predict_reg, aes(x, y), colour = "blue")
g1

