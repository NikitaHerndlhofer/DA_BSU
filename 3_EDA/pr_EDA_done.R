install.packages("tidyverse")
library(tidyverse)
library(nycflights13)
library(dplyr)

View(diamonds)
?diamonds

# Exersice 1. Base on diamonds df, explore the distribution of price, try different binwidth. Make a conclusion

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 25)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 45)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 50)

# Exersice 2. Base on diamonds df, explore the distribution of clarity and color

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = clarity))

diamonds %>% count(clarity)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = color))

diamonds %>% count(color)

# Exersice 3. How many diamonds in diamonds df are 0.99 carat? How many are 1 carat?
# What do you think is the cause of the difference?

summary(diamonds$carat)
diamonds %>% filter(between(carat, .96, 1.05)) %>%
  group_by(carat) %>% summarize(count = n())

# Exersice 4. How many missing values are there in flights df?

View(flights)
?flights

sum(is.na(flights))

# Exersice 5. What is the mean time of departure delay according to flights$dep_delay?
# What na.rm = TRUE does? Логический параметр, который указывает функции пропустить значение NA

mean_delay <- flights %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
mean_delay

# Exersice 6. Base on flights df, make new df with additional variables: dep_hour, dep_min, dep_time_formatted
# Calculate dep_hour as dep_time %/% 100, dep_min as dep_time %% 100, dep_time_formatted as dep_hour + dep_min / 60
# Analyze variance of dep_hour and dep_time_formatted.
# What happens with missing values (NA) in charts?

flights_new <- flights %>%
  
  mutate (
    cancelled = is.na(dep_time),
    dep_hour = dep_time %/% 100,
    dep_min = dep_time %% 100,
    dep_time_formatted = dep_hour + dep_min / 60
  )

View(flights_new)

# Exersice 7. Base on flights df, discover covariance between dep_delay and arr_delay,
# distance and arr_delay. Which charts are more demonstrative?

ggplot(flights, aes(dep_delay, arr_delay)) +
  geom_count()

ggplot(flights, aes(distance, arr_delay)) +
  geom_count()

# Exersice 8. Base on flights df, discover covariance between carrier and cancelled flights (hint: add variable cancelled flights)

ggplot(flights_new, aes(carrier, cancelled)) +
  geom_count() +
  coord_flip()

# Exersice 9. Base on flights df, discover covariance between carrier and dep_delay

ggplot(data = flights, mapping = aes(x = dep_delay, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = carrier), binwidth = 50)

ggplot(flights, aes(reorder(carrier, dep_delay, median, na.rm = TRUE))) +
  geom_boxplot()