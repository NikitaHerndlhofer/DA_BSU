install.packages("tidyverse")
library(tidyverse)
library(nycflights13)
library(dplyr)

View(diamonds)
?diamonds

# Exersice 1. Base on diamonds df, explore the distribution of price, try different binwidth. Make a conclusion
library(nycflights13)
library(dplyr)


# Exersice 3. How many diamonds in diamonds df are 0.99 carat? How many are 1 carat? 
# What do you think is the cause of the difference?
diamonds %>%
  count(carat==0.99)



# Exersice 5. What is the mean time of departure delay according to flights$dep_delay?
# What na.rm = TRUE does? Логический параметр, который указывает функции пропустить значение NA



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

flights_new <- flights %>%

mutate (
         cancelled = is.na(dep_time),
         dep_hour = dep_time %/% 100,
         dep_min = dep_time %% 100,
         dep_time_formatted = dep_hour + dep_min / 60
  )
 
View(flights_new)

View (my_flights)

ggplot(my_flights, aes(dep_hour)) +
  geom_bar()

# Exersice 7. Base on flights df, discover covariance between dep_delay and arr_delay, 
# distance and arr_delay. Which charts are more demonstrative?





  geom_count() +
  coord_flip()

# Exersice 9. Base on flights df, discover covariance between carrier and dep_delay



ggplot(flights, aes(reorder(carrier, dep_delay, median, na.rm = TRUE))) +
  geom_boxplot() 

# Exersice 10. Make eda for mpg df

View (mpg)
str (mpg)
?mpg
# Exersice 1. Base on mpg df, explore the distribution of displ, try different binwidth. Make a conclusion
ggplot (mpg, aes (displ))+
  geom_histogram (binwidth = 0.5)

ggplot (mpg, aes (displ))+
  geom_histogram (binwidth = 1.5)

# Exersice 2. Base on mpg df, explore the distribution of manufacturer and model

ggplot(mpg, aes(manufacturer))+
  geom_bar()

ggplot(mpg, aes(model))+
  geom_bar()  

# Exersice 3. How many cyl in mpg df are 4? How many are 8? 
# What do you think is the cause of the difference?
mpg %>%
  count(cyl==4)

mpg %>%
  count(cyl==8)

# Exersice 4. How many missing values are there in mpg df?

sum(is.na(mpg))

# Exersice 5. What is the mean time of cty according to mpg$cty?
# What na.rm = TRUE does?

by_cty <- group_by(mpg, year)
mean_cty <- summarise(by_cty, mean = mean(cty, na.rm = TRUE))
mean_cty

# Exersice 6. Base on mpg df, make new df with additional variables: date

my_mpg <- mpg %>% 
  mutate(
    cancelled = is.na(year),
    date = year %/% 100
  )


View (my_mpg)

ggplot(my_mpg, aes(date)) +
  geom_bar()
ggplot(my_mpg, aes(year)) +
  geom_bar()

# Exersice 7. Base on mpg df, discover covariance between manufacturer and cty, 
# manufacturer and hwy. Which charts are more demonstrative?

ggplot (mpg, aes (manufacturer, cty, alpha = 1/100)) +
  geom_point ()

ggplot (mpg, aes (manufacturer, hwy, alpha = 1/100)) +
  geom_point ()

ggplot (mpg, aes (manufacturer, cty))+
  geom_bin2d ()

# Exersice 8. Base on mpg df, discover covariance between model and cancelled

ggplot(my_mpg, aes(model, cancelled)) +
  geom_count() +
  coord_flip()

# Exersice 9. Base on mpg df, discover covariance between model and year

ggplot (mpg, aes (model, displ, alpha = 1/100)) +
  geom_point ()

ggplot(mpg, aes(displ, y = ..density.., colour = model))+
  geom_freqpoly(binwidth=50)

