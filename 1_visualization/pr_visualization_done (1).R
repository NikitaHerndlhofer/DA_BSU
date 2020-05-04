# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 
library(ggplot2)
?ggplot2
# View dataset "economics" from ggplot2. Read help on this dataset. 
# For each variable see meaning, type and values. 
economics$date

install.packages("lubridate")

library("lubridate")

?economics
View (economics)
str (economics)
economics$date
# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geoms point, line and smooth.
ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y = pce))
ggsave("img/plot_pce.png")

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y = psavert))
ggsave("img/plot_psavert.png")

ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = pop))
ggsave("img/plot_pop.png")

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = unemploy))
ggsave("img/plot_unemploy.png")
# Exersice 2. Display pce and psavert on one plot.
ggplot(data = economics) +
  geom_point(mapping = aes(x = pce, y =psavert))
ggsave("img/plot_pcepsavert.png")

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =psavert))+geom_point(mapping = aes(x = date, y = pce))
ggsave("img/plot_pcepsavert2.png")

# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).
ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y =unemploy, size = uempmed))
ggsave("img/exer3.png")

# Exersice 4. Run the following code line by line and explain what it does.
economics$date

year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

# Exersice 5. Plot for x = uempmed, y = unemploy facet_wrap on decade.
ggplot(data = my_economics) +
  geom_point(mapping = aes(x = uempmed, y = unemploy)) +
  facet_wrap(~ decade)
ggsave("img/exer5.png")
# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.
ggplot(data = my_economics) +
  geom_smooth(mapping = aes(x = uempmed, y = unemploy, color = decade))
ggsave("img/exer6.png")
# Exersice 7. Plot histogram for unemploy for each decade.
ggplot(economics, aes(unemploy)) +
  geom_histogram()
ggsave("img/exer7_histogram_unemploy1.png")

ggplot(my_economics, aes(unemploy, fill = decade)) +
  geom_histogram()
ggsave("img/exer7_histogram_unemploy2.png")

ggplot(my_economics, aes(unemploy)) +
  geom_histogram()+
  facet_wrap(~ decade)
ggsave("img/exer7_histogram_unemploy3.png")

# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.
ggplot(my_economics, aes(x = decade, y = unemploy)) +
  geom_boxplot() +
  coord_flip()
ggsave("img/exer8.png")

ggplot(my_economics) +
  stat_summary(
    mapping = aes(x = decade, y = unemploy),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
ggsave("img/exer8_stat_summary.png")
# Exersice 9. Set working directory and save one of the plots to it

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct



