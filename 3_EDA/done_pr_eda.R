# Exersice 1. Base on diamonds df, explore the distribution of price, try different binwidth. Make a conclusion

ggplot(diamonds, aes(price)) + 
  geom_histogram()

ggplot(diamonds, aes(price)) + 
  geom_histogram(binwidth = 50)

ggplot(diamonds, aes(price)) + 
  geom_freqpoly()

ggplot(diamonds, aes(price)) + 
  geom_freqpoly(binwidth = 50)

count_price <- diamonds %>%
  count(cut_width(price,50))

count_price

# Exersice 2. Base on diamonds df, explore the distribution of clarity and color

ggplot(diamonds, aes(clarity)) +
  geom_bar()

diamonds %>%
  count(clarity)

ggplot(diamonds, aes(color)) +
  geom_bar()

diamonds %>%
  count(color)

# Exersice 3. How many diamonds in diamonds df are 0.99 carat? How many are 1 carat? 
# What do you think is the cause of the difference?

unusual <- diamonds %>% 
  filter(carat == 0.99) 
count(unusual)

unusual1 <- diamonds %>% 
  filter(carat == 1) 
count(unusual1)

ggplot(diamonds, aes(carat)) +
  geom_bar()

ggplot(diamonds, aes(carat)) +
  geom_boxplot()

#2 вариант
carat_near_1 <- diamonds %>% 
  filter(carat > 0.9 & carat <1.1) %>%
  select(carat) %>%
  arrange(carat)

# Exersice 4. How many missing values are there in flights df?
sum(is.na(flights))

# Exersice 5. What is the mean time of departure delay according to flights$dep_delay?
# What na.rm = TRUE does?

mean(flights$dep_delay, na.rm = TRUE)

# Exersice 6. Base on flights df, make new df with additional variables: dep_hour, dep_min, dep_time_formatted
# Calculate dep_hour as dep_time %/% 100, dep_min as dep_time %% 100, dep_time_formatted as dep_hour + dep_min / 60
# Analyze variance of dep_hour and dep_time_formatted. 
# What happens with missing values (NA) in charts?

flights_my <- flights

flights_my <-mutate(flights_my,
       cancelled = is.na(dep_time),
       dep_hour = dep_time %/% 100,
       dep_min = dep_time %% 100,
       dep_time_formatted = dep_hour + dep_min / 60
)

View(flights_my)

ggplot(flights_my, aes(dep_time_formatted)) + 
  geom_freqpoly(binwidth = 1/4)

ggplot(flights_my, aes(dep_hour)) + 
  geom_freqpoly(binwidth = 1/4)

#второй вариант
flights_modif <- flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    dep_hour = dep_time %/% 100,
    dep_min = dep_time %% 100,
    dep_time_formatted = dep_hour + dep_min / 60
  ) 

  ggplot(flights_modif, aes(dep_hour)) + 
  geom_bar()

 # Warning message:
 # Removed 8255 rows containing non-finite values (stat_count).

  ggplot(flights_modif, aes(dep_time_formatted)) + 
    geom_histogram()
  
# Exersice 7. Base on flights df, discover covariance between dep_delay and arr_delay, 
# distance and arr_delay. Which charts are more demonstrative?

ggplot(flights, aes(dep_delay, arr_delay, alpha = 1/100)) +
  geom_point()

ggplot(flights, aes(distance, arr_delay, alpha = 1/100)) +
  geom_count()

ggplot(flights, aes(distance, arr_delay)) +
  geom_bin2d()

ggplot(flights, aes(distance, arr_delay)) +
  geom_boxplot(aes(group = cut_width(distance,500)), varwidth = TRUE)

# Exersice 8. Base on flights df, discover covariance between carrier and cancelled flights (hint: add variable cancelled flights)

flights_modif <- flights %>%
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE))

ggplot(flights_modif, aes(carrier, cancelled)) +
  geom_count() +
  coord_flip()

# Exersice 9. Base on flights df, discover covariance between carrier and dep_delay

ggplot(flights, aes(dep_delay, y = ..density.., colour = carrier)) +
  geom_freqpoly(binwidth = 50)

ggplot(flights, aes(reorder(carrier, dep_delay, median, na.rm = TRUE))) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,200)) 

# Exersice 10. Make eda for mpg df


