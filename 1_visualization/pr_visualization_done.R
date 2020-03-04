library(ggplot2)

View(economics)

str(economics)

# psavert - personal savings rate
ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = psavert))

ggplot(data = economics) + 
  geom_line(mapping = aes(x = date, y = psavert))

ggplot(data = economics) + 
  geom_smooth(mapping = aes(x = date, y = psavert))

# pce - personal consumption expenditures, in billions of dollars
ggplot(data = economics) + 
  geom_smooth(mapping = aes(x = date, y = pce))

ggplot(data = economics) + 
  geom_smooth(mapping = aes(x = pce, y = psavert))

ggplot(data = economics) + 
  geom_point(mapping = aes(x = pce, y = psavert))


# three vars on one plot
ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y =unemploy, size = uempmed)) 

# install.packages("lubridate")
library(lubridate)

# economics$date

# year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

# class(my_economics$decade) 

my_economics$decade <- as.factor(my_economics$decade)

# class(my_economics$decade)

# facet
ggplot(data = my_economics) + 
  geom_smooth(mapping = aes(x = uempmed, y = unemploy)) +
  facet_wrap(~ decade)

# draw a separate object for each unique value of decade
ggplot(data = my_economics) + 
  geom_smooth(mapping = aes(x = uempmed, y = unemploy, color = decade)) +
  
# plot a histogram for unemploy for each decade
ggplot(my_economics, aes(unemploy)) +
  geom_histogram() +
  facet_wrap(~ decade)
  
# plot geom_boxplot and stat_summary for unemploy for each decade 
ggplot(my_economics, aes(decade, unemploy)) + 
  geom_boxplot()
  

ggplot(data = my_economics) + 
  stat_summary(
    mapping = aes(x = decade, y = unemploy),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )


