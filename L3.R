L3

# Exersice 1. Base on diamonds df, explore the distribution of price, try different binwidth. Make a conclusion

View(diamonds)
ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 60)

diamonds %>%
  count(cut_width(price, 60))

# Exersice 2. Base on diamonds df, explore the distribution of clarity and color

ggplot(diamonds, aes(clarity)) +
  geom_bar()

diamonds %>% count(clarity)

ggplot(diamonds, aes(color)) +
  geom_bar()

diamonds %>% count(color)

# Exersice 3. How many diamonds in diamonds df are 0.99 carat? How many are 1 carat? 
# What do you think is the cause of the difference?

diamonds %>% filter(between(carat, .99, 1)) %>%
  group_by(carat) %>% summarize(count = n())


# Exersice 4. How many missing values are there in flights df?
sum(is.na(flights))
#46595

# Exersice 5. What is the mean time of departure delay according to flights$dep_delay?
# What na.rm = TRUE does?

mean_d <- flights %>%
  summarise(flights = mean(dep_delay, na.rm = TRUE))
mean_d

# Exersice 6. Base on flights df, make new df with additional variables: dep_hour, dep_min, dep_time_formatted
# Calculate dep_hour as dep_time %/% 100, dep_min as dep_time %% 100, dep_time_formatted as dep_hour + dep_min / 60
# Analyze variance of dep_hour and dep_time_formatted. 
# What happens with missing values (NA) in charts?

flights_modif <- flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    dep_hour = dep_time %/% 100,
    dep_min = dep_time %% 100,
    dep_time_formatted = dep_hour + dep_min / 60
  )
View(flights)

# Exersice 7. Base on flights df, discover covariance between dep_delay and arr_delay, 
# distance and arr_delay. Which charts are more demonstrative?

ggplot(flights, aes(dep_delay, arr_delay, alpha=1 / 100)) +
  geom_point()

ggplot(flights, aes(distance, arr_delay, alpha=1 / 100)) +
  geom_point()

ggplot(flights, aes(distance, arr_delay)) +
  geom_bin2d()

# Exersice 8. Base on flights df, discover covariance between carrier and cancelled flights (hint: add variable cancelled flights)

flights_modif <- flights %>%
  mutate(cancelled = ifelse(is.na(dep_time), TRUE, FALSE))

ggplot(flights_modif, aes(carrier, cancelled)) +
  geom_count() +
  coord_flip()

# Exersice 9. Base on flights df, discover covariance between carrier and dep_delay

ggplot(flights, aes(dep_delay, y = ..density.., colour = carrier)) + 
  geom_freqpoly(binwidth = 500)


#два варианта с разными ошибками
ggplot(flights, aes(x = reorder(carrier, dep_delay, median, rm(list = stat_bin(8255))), cancelled)) +
  geom_boxplot()+
    coord_cartesian(ylim = c(0, 200))


ggplot(flights, aes(x = reorder(carrier, dep_delay, median), cancelled)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 200))


View(flights)
# Exersice 10. Make eda for mpg df

View (mpg)
str (mpg)
?mpg

ggplot (mpg, aes (displ))+
  geom_freqpoly (binwidth = 1.5)

ggplot(mpg, aes(model)) +
  geom_bar()

mpg %>%
  count(model)

displ <- mpg %>%
  filter(displ == 1.8)
count(displ)

ggplot(mpg, aes(displ)) +
  geom_boxplot()

sum(is.na(mpg))


 mean(mpg$hwy, na.rm = TRUE)
 
 ggplot (mpg, aes (displ, hwy, alpha = 1/100)) +
   geom_point()
 
 # covariance between year and hwy
 
 ggplot(mpg, aes(hwy, year)) +
   geom_count() +
   coord_flip()
 
 