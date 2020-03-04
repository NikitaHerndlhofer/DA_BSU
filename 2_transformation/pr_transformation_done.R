library(ggplot2)
library(lubridate)
library(dplyr)

View(economics)
my_economics <- economics


# Exersice 1. How many missing values are there in the dataset "economics" (from ggplot2)?
sum(is.na(my_economics))
# [1] 0


# Exersice 2. Check the type of variables in "economics"
class(economics$date)



# Exersice 3. Find observations from "economics" that

# relate to 2015
economics_2015 <- filter(economics, year(date) == 2015)

# relate to January (use nrow() to know, how many rows you have found)
filter(economics, month(date) == 1)
nrow(filter(economics, month(date) == 1))

# psavert >= 12 and pce > 600
filter(economics, psavert >= 12 & pce > 600)

# psavert < 5 or psavert > 12
filter(economics, psavert < 5 | psavert > 12)

# psavert between 10 and 13 (inclusive) - use between()
filter(economics, between(psavert, 10, 13))


# Exersice 4. Which 5 rows of "economics" contain the highest unemploy values?
# What are these 5 unemploy values and corresponding dates?
head(arrange(economics, desc(unemploy)), 5)

head(arrange(economics, desc(unemploy)), 5)$unemploy

head(arrange(economics, desc(unemploy)), 5)$date


# Exersice 5. Create "my_economics" as a copy of "economics". 
# Use mutate() to add a new column "year" (hint: see pr_visualization).
# View "my_economics" to make sure the column "year" has been added
# Compare two ways we used to create a column: this one and in pr_visualization. 
my_economics <- economics        
my_economics <- mutate(my_economics, year = year(date))


# Exersice 6.Use group_by() and summarise() to calculate mean number of unemployed per year in "my_economics" 
# Save the result in "year_unemploy" dataset
by_year <- group_by(my_economics, year)
year_unemploy <- summarise(by_year, mean_unemploy = mean(unemploy))
year_unemploy

# Exersice 7. Use pipe to get the same result as in exersice 6
year_unemploy <- my_economics %>%
  group_by(year) %>%
  summarise(mean_unemploy = mean(unemploy))
year_unemploy


