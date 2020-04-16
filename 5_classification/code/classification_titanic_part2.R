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
# setwd("D:/R projects/ds-courses/BSU/classification")

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



# Feature engeneering
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_classic()



# Another way to check if there is a monotonic dependence of Survived from Fsize
full[1:891,c("Fsize", "Survived")] %>% 
  group_by(Fsize) %>%
  summarise(mean = mean(as.integer(as.character(Survived))))

# Fsize  mean
# <dbl> <dbl>
#   1     1 0.304
# 2     2 0.553
# 3     3 0.578
# 4     4 0.724
# 5     5 0.2  
# 6     6 0.136
# 7     7 0.333
# 8     8 0    
# 9    11 0 

# Conclusion: the dependence is not monotonous, then it is reasonable to try to discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

full$FsizeD <- as.factor(full$FsizeD)
full %<>% select(-Fsize)









