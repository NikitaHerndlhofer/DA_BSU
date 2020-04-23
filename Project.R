# Adult income dataset 

library(readr)
library(ggplot2)
library(dplyr)
library(h2o)
library(xgboost)

adult <- read_csv("Project/data/adult.csv")
View(adult)
str(adult)

h2o.removeAll() 
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

barplot(table(adult$age))

##  Working schedule

table(adult$hours.per.week)

adult$hours.per.week <- cut(adult$hours.per.week, c(0,25,40,60,168), labels = c("Part-time", "Full-time", "Over-time", "Too-much"), ordered=TRUE)

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

adult$income <- as.factor(adult$income)

# Dealing with Missing Data

library(Amelia) # program for missing data
missmap(adult, y.at = 1, y.labels = "", col = c("red", "black"), legend = FALSE)
adult <- na.omit(adult)
missmap(adult, y.at = 1, y.label = "", legend = FALSE, col = c("red", "black"))

# Exploratory Data Analysis

ggplot(adult, aes(age)) + geom_histogram(aes(fill = income),
                                         binwidth = 1, stat = "count")

ggplot(adult, aes(hours.per.week)) + geom_histogram(aes(fill = income),
                                         binwidth = 1, stat = "count")

ggplot(adult, aes(x=education)) + 
  geom_bar() + 
  facet_grid(~sex)

ggplot(adult, aes(x=income, y=1, fill=marital.status)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~sex)

# Building the Model

install.packages("caret")
library(caret)
set.seed(42)


========

  
predictors <- colnames(adult)[1:10]

response <- "income"  
  
#  
localH2O = h2o.init(nthreads=-1)

adult <- as.data.frame(adult)

hex_adult <- as.h2o(adult)

## Splits data into 2 parts

adult.split <- h2o.splitFrame(data = hex_adult, ratios = 0.8, seed = 42)
train <- hex_adult.split[[1]]
valid <- hex_adult.split[[2]]
View(adult)



## Model 


=======