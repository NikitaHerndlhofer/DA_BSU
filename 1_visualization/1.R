# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 

# View dataset "economics" from ggplot2. Read help on this dataset. 
# For each variable see meaning, type and values. 

library(ggplot2)
install.packages("lubridate")
library(lubridate)

View(economics)

economics
?economics

# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geoms point, line and smooth.

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = pce))

ggsave("img/exercise_1_1.png")

ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = pop))

ggsave("img/exercise_1_2.png")

ggplot(data = economics) + 
  geom_smooth(mapping = aes(x = date, y = psavert))

ggsave("img/exercise_1_3.png")

# Exersice 2. Display pce and psavert on one plot.

ggplot(data = economics) + 
  geom_point(mapping = aes(x = pce, y = psavert))

ggplot(data = economics) + 
  geom_smooth(mapping = aes(x = pce, y = psavert))

ggsave("img/exercise_2.png")

# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = unemploy, size = uempmed))

ggsave("img/exercise_3.png")

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

?facet_wrap

ggplot(data = my_economics) + 
  geom_point(mapping = aes(x = uempmed, y = unemploy)) +
  facet_wrap(~ decade, ncol=7)

ggsave("img/exercise_5.png")

# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.

?geom_smooth

ggplot(data = my_economics) +
  geom_smooth(mapping = aes(x = uempmed, y = unemploy, color = decade)) 

ggsave("img/exercise_6.png")

# Exersice 7. Plot histogram for unemploy for each decade.

ggplot(my_economics, aes(unemploy)) +
  geom_histogram() +
  facet_wrap(~ decade)

ggsave("img/exercise_7.png")

# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.

ggplot(my_economics, aes(x = decade, y = unemploy)) +
  geom_boxplot() +
  coord_flip()

ggsave("img/exercise_8_boxplot.png")

ggplot(my_economics) +
  stat_summary(
    mapping = aes(x = decade, y = unemploy),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
    )

ggsave("img/exercise_8_stat_summary.png")

# Exersice 9. Set working directory and save one of the plots to it

# Exersice 9. Save one of the plots to directory img

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct



