
library(ggplot2)
library(lubridate)
install.packages('tidyverse')
library(dplyr)


?economics
economics


# Exersice 1. How many missing values are there in the dataset "economics" (from ggplot2)?

x <- NA
is.na(x)
sum(is.na(economics))

# Exersice 2. Check the type of variables in "economics"

class(economics)
str(economics)

# Exersice 3. Find observations from "economics" that

# relate to 2015

filter(economics, year(date) == 2015)

year2015 <- filter(economics, year(date) == 2015)
year2015

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

desc_economics <- arrange(economics, desc(unemploy))
desc_economics <- head(desc_economics,5)

select(desc_economics, unemploy, date)

# Exersice 5. Create "my_economics" as a copy of "economics". 

my_economics <- economics
View(my_economics)

# Use mutate() to add a new column "year" (hint: see pr_visualization).

my_economics <- mutate(my_economics,
       year = year(date)
)

# View "my_economics" to make sure the column "year" has been added

View(my_economics)

# Compare two ways we used to create a column: this one and in pr_visualization. 



# Exersice 6.Use group_by() and summarise() to calculate mean number of unemployed per year in "my_economics" 
# Exersice 7. Use pipe to get the same result as in exersice 6

year_unemploy <- my_economics %>%
  group_by(year) %>%
  summarise(year_unemploy = mean(unemploy, na.rm = TRUE))
year_unemploy

tail(year_unemploy)

# Save the result in "year_unemploy" dataset





