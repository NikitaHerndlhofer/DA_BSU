# Book: R for Data Science https://r4ds.had.co.nz
# Chapter 5 

# install.packages('nycflights13')
library(nycflights13)
install.packages('tidyverse')
library(dplyr)



?flights
flights

# str(), dim(), View(), summary()

head(flights,3)
tail(flights)

# Type of variables
class(flights)
class(flights$day)


# Main types of vectors
# Logical, integer, double (double precision floating point numbers), 
# numeric (integer and double), characters, factors


# Functions to check variable type or convert between different types
# Check: is.numeric(), is.logical(), is.character(), is.factor() ...
# Convert: as.logical(), as.integer(), as.double(), as.character(), as.factor() ...


# Create vector
# https://stackoverflow.com/questions/1741820/what-are-the-differences-between-and-assignment-operators-in-r
x <-  c(1, 2, 3) # x = c(1, 2, 3)

class(x)

is.numeric(x)

is.integer(x)

x <- as.factor(x)
class(x)


# Create dataframe
squares <- data.frame(
  size = c('small', 'big', 'medium'),
  edge = c('dotted', 'striped', 'normal'),
  color = c('green', 'yellow', 'green')
)

View(squares)

str(squares)

# What are types of variables in flights?


# 5 key functions of dplyr
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).

# 1. Filter rows with filter()------------------------------------------------
filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

# comparisons >, >=, <, <=, ==,!=

sqrt(2) ^ 2 == 2
# [1] FALSE

near(sqrt(2) ^ 2,  2)
# [1] TRUE

# logical operators: & - logical "and", | - logical "or"
# see more logical operators ihttps://r4ds.had.co.nz

filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

# missing values NA ("not availables")

NA == NA
# [1] NA ????

x <- NA
is.na(x)
# [1] TRUE

# How many missing values are there in flights?
sum(is.na(flights))

# 2. Arrange (order) rows with arrange()--------------------------------------

arrange(flights, year, month, day)

# use desc() to re-order
arrange(flights, desc(dep_delay))

# missing values are always sorted at the end

tail(arrange(flights, desc(dep_delay)),3)

# 3. Select columns with select()---------------------------------------------
# Actual for datasets with many columns to narrowing in on the variables.

select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day))

select(flights, time_hour, air_time, everything())


# 4. Add new variables with mutate()-------------------------------------------

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

# Useful creation function +, -, *, /, ^, %/% (integer division) and %% (remainder)

# 5. Grouped summaries with summarise()----------------------------------------
by_day <- group_by(flights, year, month, day)
mean_delay <- summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
mean_delay

# Combining multiple operations with the pipe----------------------------------
mean_delay <- flights %>%
  group_by(year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
mean_delay

# What will we get if use the previous block of commands without "na.rm = TRUE"?

# 6. Sum up--------------------------------------------------------------------
# Let's have a look through the code and sum up what we have learned






