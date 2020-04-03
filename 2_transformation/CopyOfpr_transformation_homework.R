
# Exersice 1. How many missing values are there in the dataset "economics" (from ggplot2)?

sum(is.na(economics))
View (economics)

# Exersice 2. Check the type of variables in "economics"

class (economics)
class (economics$date)
class (economics$pce)
class (economics$pop)
class (economics$psavert)
class (economics$uempmed)
class (economics$unemploy)

# Exersice 3. Find observations from "economics" that

# relate to 2015
filter(economics, year(date) == 2015)

# relate to January (use nrow() to know, how many rows you have found)
nrow (filter (economics, (month (date) == 01)))

# psavert >= 12 and pce > 600
filter(economics, psavert >= 12, pce > 600)

# psavert < 5 or psavert > 12
filter(economics, psavert < 5| psavert > 12)

# psavert between 10 and 13 (inclusive) - use between()

between10 <- filter(economics, between (psavert, 10, 13))

# Exersice 4. Which 5 rows of "economics" contain the highest unemploy values?
head(arrange(economics, desc(unemploy)),5)

# What are these 5 unemploy values and corresponding dates?

head(arrange(economics, desc(unemploy)),5)$date

# Exersice 5. Create "my_economics" as a copy of "economics". 

my_economics <- economics

# Use mutate() to add a new column "year" (hint: see pr_visualization).
my_economics <- mutate(my_economics,
       year = year(economics$date))

# View "my_economics" to make sure the column "year" has been added
View (my_economics)
str (economics)
# Compare two ways we used to create a column: this one and in pr_visualization. 


# Exersice 6.Use group_by() and summarise() to calculate mean number of unemployed per year in "my_economics" 

year_unemploy <- my_economics %>%
group_by(year) %>%
summarise(year_unemploy = mean(unemploy))
year_unemploy

#Save the result in "year_unemploy" dataset

# Exersice 7. Use pipe to get the same result as in exersice 6


