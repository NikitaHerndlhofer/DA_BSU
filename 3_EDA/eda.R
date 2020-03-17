# Book: R for Data Science https://r4ds.had.co.nz
# Chapter 7

# Use visualisation and transformation to explore your data in a systematic way - exploratory data analysis (EDA)
# EDA iterative cycle:
# 1) Generate questions about your data.
# 2) Search for answers by visualising, transforming, and modelling your data.
# 3) Use what you learn to refine your questions and/or generate new questions.

# install.packages("tidyverse")
library(tidyverse)

View(diamonds)
?diamonds

# Categorical & continues variables. Intuition
# Categorical variable takes one of a relativly small set of values
# Continues variable takes any of an infinite set of ordered values

# Determine categorical and continues variables in diamonds? 


# 1. Variation

# 1.1 Categorical variable - geom_bar(), count()
ggplot(diamonds, aes(cut)) +
  geom_bar()

diamonds %>%
  count(cut)

# Note - short form is used for simplicity (data, mapping are omitted)

# 1.2 Continues variable - geom_histogram(), count(), geom_freqpoly()
ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))

# <TODO> What are the min and max values of diamonds$carat? 
# Discover min(), max() and summary()


# Why oservations with carat>=3 were not visualised on the above histogram?  

# one can zoom to small values of the y-axis to visualize them
ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0,1000)) 


# Multiple histograms in the same plot
# Use geom_freqpoly() to avoid overlapping (uses count() the same as geom_histogram())
ggplot(diamonds, aes(carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)
 

# 1.3 Typical values
# The histogram below suggests several interesting questions:
# 1) Why are there more diamonds at whole carats and common fractions of carats?
# 2) Why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)


# 1.4 Unusual values
# Outliers are observations that are unusual; data points that don’t seem to fit the pattern. 
# Sometimes outliers are data entry errors; other times outliers suggest important new science.
# When you have a lot of data, outliers are sometimes difficult to see in a histogram (see example above, where we limited y).

# Let's take the distribution of the y variable from the diamonds dataset. 
# The only evidence of outliers is the unusually wide limits on the x-axis.
ggplot(diamonds, aes(y)) + 
  geom_histogram(binwidth = 0.5)

# <TODO> How can we visualize outliers?



# What three unusual values are?

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)

unusual

# We know that diamonds can’t have a width of 0mm, so these values must be incorrect.
# We might also suspect that measurements of 32mm and 59mm are implausible: those diamonds are over an inch long, 
# but don’t cost hundreds of thousands of dollars!

# It’s good practice to repeat your analysis with and without the outliers. 
# If they have minimal effect on the results, and you can’t figure out why they’re there, 
# it’s reasonable to replace them with missing values, and move on. 
# However, if they have a substantial effect on your results, you shouldn’t drop them without justification. 
# You’ll need to figure out what caused them (e.g. a data entry error) and disclose that you removed them in your write-up



# 2. Missing values
# What to do with unusual values?

# Option 1: drop the enture row - not recommended
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# Option 2: replacing the unusual values with missing values
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))


# When you print variable with missing value ggplot gives you a warning
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

# To suppress that warning, set na.rm = TRUE:
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

# One can make new variable with is.na() and use it in analysys.
# E.g. in nycflights13::flights, missing values in the dep_time variable indicate that the flight was cancelled.
# So you might want to compare the scheduled departure times for cancelled and non-cancelled times.
flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(aes(sched_dep_time, colour = cancelled)) + 
    geom_freqpoly(binwidth = 1/4)

# There are many more non-cancelled flights than cancelled flights, therefor the plot isn't great



# 3. Covariation. Intuition

# Covariation is the tendency for the values of two or more variables to vary together in a related way.
# The best way to spot covariation is to visualise the relationship between two or more variables.


# 3.1 A categorical and continuous variable - geom_freqpoly(), box_plot()

# <TODO> Use geom_freaqpoly() to visualize price variations for different cuts (binwidth = 500)



# It’s hard to see the difference in distribution because the overall counts differ so much:
# <TODO> Use geom_bar() or count() to show the counts differ for different cuts




# Solution: to make the comparison easier, instead of displaying count, we’ll display density,
# which is the count standardised so that the area under each frequency polygon is one.

ggplot(diamonds, aes(price, y = ..density.., colour = cut)) + 
  geom_freqpoly(binwidth = 500)

# <TODO> How average price depends on cut value?

# A boxplot can answer this question more pricesely.
# Boxplot explanation: https://en.wikipedia.org/wiki/Box_plot#cite_note-4, https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51

ggplot(diamonds, aes(cut, price)) +
  geom_boxplot()

ggplot(diamonds, aes(x = reorder(cut, price, FUN = median), price)) +
  geom_boxplot()

ggplot(diamonds, aes(x = reorder(cut, price, FUN = median), price)) +
  geom_boxplot() +
  coord_flip()



# 3.2 Two categorical variables
# To visualise the covariation between categorical variables, you’ll need to count the number of observations for each combination. 
# geom_count(), count()

ggplot(diamonds, aes(cut, color)) +
  geom_count()

color_cut <- diamonds %>%
  count(color, cut)

# <TODO> What kind of dependency between color and cut have you mention in the plot and color_cut?


# 3.3 Two continuous variables

# <TODO> How did you visualize the covariation between two continues variables in pr_visualization?
# <TODO> Visualize a relationship between the carat size and price of a diamond




# Scatterplots become less useful as the size of your dataset grows, because points begin to overplot, 
# One way to fix the problem - using the alpha aesthetic to add transparency.

ggplot(diamonds, aes(carat, price, alpha = 1 / 100)) + 
  geom_point()

# Another solution - geom_bin2d() to bin in two dimensions
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()


# Another option is to bin one continuous variable so it acts like a categorical variable
ggplot(diamonds, aes(carat, price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.3)))

# With varwidth = TRUE width of the boxplot proportional to the number of points
ggplot(diamonds, aes(carat, price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.3)), varwidth = TRUE)


# 4. Patterns and models

# A scatterplot of Old Faithful eruption lengths versus the wait time between eruptions shows a pattern: 
# longer wait times are associated with longer eruptions. 
# The scatterplot also displays the two clusters that we noticed above

ggplot(faithful, aes(eruptions, waiting)) + 
  geom_point()







