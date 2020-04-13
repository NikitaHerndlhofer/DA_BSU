# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 

# View dataset "economics" from ggplot2. Read help on this dataset. 
# For each variable see meaning, type and values. 

# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geoms point, line and smooth.
ggplot(data = economics)+
  geom_point(mapping = aes(x = date, y = psavert))
ggplot(data = economics)+
  geom_line(mapping = aes(x = date, y = psavert))
ggplot(data = economics)+
  geom_smooth(mapping = aes(x=date, y=psavert))
# Exersice 2. Display pce and psavert on one plot.
ggplot(data = economics, aes(x =date, y =psavert))+ geom_line()
# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).
ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = unemploy, size = uempmed))
# Exersice 4. Run the following code line by line and explain what it does.

economics$date

# install.packages('lubridate')

library(lubridate)

year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

# Exersice 5. Plot for x = uempmed, y = unemploy facet_wrap on decade.
ggplot(data = economics)+
  geom_point(mapping = aes(x = uempmed, y = unemploy))+
  facet_wrap(~my_economics$decade)

# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.
ggplot(data = economics)+
  geom_smooth(mapping = aes(x = uempmed, y = unemploy))+
  facet_wrap(~my_economics$decade)
# Exersice 7. Plot histogram for unemploy for each decade.
ggplot(economics,aes(unemploy))+
  geom_histogram()+
  facet_wrap(~my_economics$decade)

# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.
<<<<<<< HEAD:1_visualization/pr_visualizationdone.R
ggplot(economics, aes(unemploy))+
  geom_boxplot()+
  facet_wrap(~my_economics$decade)

ggplot(economics)+
  stat_summary(
    mapping = aes(x = my_economics$decade, y = unemploy),
    fun.xmin = min,
    fun.xmax = max,
    fun.x = median
  )
  
# Exersice 9. Set working directory and save one of the plots to it
=======

# Exersice 9. Save one of the plots to directory img
>>>>>>> e69dd27f7bb30eb4a5db73de5fa3f1afa7edadee:1_visualization/pr_visualization.R

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct

