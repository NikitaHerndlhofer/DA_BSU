# Exersice 1. Base on diamonds df, explore the distribution of price, try different binwidth. Make a conclusion
library(nycflights13)
library(dplyr)

ggplot (diamonds, aes (price))+
  geom_histogram (binwidth = 0.5)

ggplot (diamonds, aes (price))+
  geom_histogram (binwidth = 1.5)

# Exersice 2. Base on diamonds df, explore the distribution of clarity and color

ggplot(diamonds, aes(clarity))+
  geom_bar()

ggplot(diamonds, aes(color))+
  geom_bar()  

# Exersice 3. How many diamonds in diamonds df are 0.99 carat? How many are 1 carat? 
# What do you think is the cause of the difference?
diamonds %>%
  count(carat==0.99)

diamonds %>%
  count(carat==1)

# Exersice 4. How many missing values are there in flights df?

sum(is.na(flights))
View (flights)

# Exersice 5. What is the mean time of departure delay according to flights$dep_delay?
# What na.rm = TRUE does?

by_day <- group_by(flights, year, month, day)
mean_delay <- summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
mean_delay

# Exersice 6. Base on flights df, make new df with additional variables: dep_hour, dep_min, dep_time_formatted
# Calculate dep_hour as dep_time %/% 100, dep_min as dep_time %% 100, dep_time_formatted as dep_hour + dep_min / 60
# Analyze variance of dep_hour and dep_time_formatted. 
# What happens with missing values (NA) in charts?
my_flights <- flights
View (my_flights)


my_flights <- flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    dep_hour = dep_time %/% 100,
    dep_min = dep_time %% 100,
    dep_time_formatted = dep_hour + dep_min / 60
  )


View (my_flights)

ggplot(my_flights, aes(dep_hour)) +
  geom_bar()

# Exersice 7. Base on flights df, discover covariance between dep_delay and arr_delay, 
# distance and arr_delay. Which charts are more demonstrative?

ggplot (flights, aes (dep_delay, arr_delay, alpha = 1/100)) +
  geom_point ()

ggplot (flights, aes (distance, arr_delay, alpha = 1/100)) +
  geom_point ()

ggplot (flights, aes (distance, arr_delay))+
  geom_bin2d ()

ggplot (flights, aes (distance, arr_delay)) +
  geom_boxplot (aes(group = cut_width (distance, 500)), varwidth = TRUE)

# Exersice 8. Base on flights df, discover covariance between carrier and cancelled flights (hint: add variable cancelled flights)

my_flights <- flights %>%
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE))

ggplot(my_flights, aes(carrier, cancelled)) +
  geom_count() +
  coord_flip()

# Exersice 9. Base on flights df, discover covariance between carrier and dep_delay

ggplot (flights, aes (carrier, dep_delay, alpha = 1/100)) +
  geom_point ()

ggplot(flights, aes(dep_delay, y = ..density.., colour = carrier))+
  geom_freqpoly(binwidth=50)

ggplot(flights, aes(reorder(carrier, dep_delay, median, na.rm = TRUE))) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,100)) 


# Exersice 10. Make eda for mpg df

View (mpg)
str (mpg)
?mpg




