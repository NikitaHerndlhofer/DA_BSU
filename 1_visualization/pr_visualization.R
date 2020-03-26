# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 

# View dataset "economics" from ggplot2. Read help on this dataset. 
# For each variable see meaning, type and values. 

economics$date

install.packages("lubridate")

library("lubridate")

year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geomspoint, line and smooth.

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =pop))
ggsave("img/plot_point_pop.png")

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =psavert))
ggsave("img/plot_point_psavert.png")

ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y =pop))
ggsave("img/plot_line_pop.png")

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y =psavert))
ggsave("img/plot_smooth_psavert.png")


# Exersice 2. Display pce and psavert on one plot.

ggplot(data = economics) +
  geom_point(mapping = aes(x = pce, y =psavert))
ggsave("img/plot_pce_psavert.png")

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =psavert))+geom_point(mapping = aes(x = date, y = pce))
ggsave("img/plot_pce+psavert.png")


# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =unemploy, size = uempmed))
ggsave("img/exercise_3.png")


# Exersice 4. Run the following code line by line and explain what it does.

economics$date

year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

#создали столбец данных - "декада", класс - "числа" поменяли на класс - "фактор"


# Exersice 5. Plot for x = uempmed, y = unemploy facet_wrap on decade.

ggplot(data = my_economics) +
  geom_point(mapping = aes(x = uempmed, y = unemploy)) +
  facet_wrap(~ decade)
ggsave("img/ex4_facwt_wrap.png")


# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.

ggplot(data = my_economics) +
  geom_smooth(mapping = aes(x = uempmed, y = unemploy, color = decade))
ggsave("img/ex6.png")


# Exersice 7. Plot histogram for unemploy for each decade.

ggplot(economics, aes(unemploy)) +
  geom_histogram()
ggsave("img/ex7_histogram_unemploy.png")

ggplot(my_economics, aes(unemploy, fill = decade)) +
  geom_histogram()
ggsave("img/ex7_histogram_unemploy.png")

ggplot(my_economics, aes(unemploy)) +
  geom_histogram()+
  facet_wrap(~ decade)


# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.

ggplot(my_economics) +
  stat_summary(
    mapping = aes(x = decade, y = unemploy),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
ggsave("img/ex8_stat_summary_unemploy.png")


# Exersice 9. Set working directory and save one of the plots to it

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct



