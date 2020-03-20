# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 

# View dataset "economics" from ggplot2. Read help on this dataset. 
View (economics)
# For each variable see meaning, type and values. 
str (economics)
# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geoms point, line and smooth.

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = pce))

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = pop))

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = psavert))

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = uempmed))

ggplot(data = economics) + 
  geom_point(mapping = aes(x = date, y = unemploy))

ggplot(data = economics) + 
  geom_line (mapping = aes(x = date, y = unemploy))

ggplot(data = economics) + 
  geom_line (mapping = aes(x = date, y = uempmed))

ggplot(data = economics) + 
  geom_line (mapping = aes(x = date, y = psavert))

ggplot(data = economics) + 
  geom_line (mapping = aes(x = date, y = pop))

ggplot(data = economics) + 
  geom_line (mapping = aes(x = date, y = pce))

ggplot(data = economics) +
  geom_line(mapping = aes(x = psavert, y = pce))

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = unemploy))

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = uempmed))

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = psavert))

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = pop))

ggplot(data = economics) +
  geom_smooth(mapping = aes(x = date, y = pce))

# Exersice 2. Display pce and psavert on one plot.

ggplot(data = economics) +
  geom_point(mapping = aes(x = pce, y = psavert))

ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y = pce)) +
  facet_wrap(~ psavert)

# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).

ggplot(data = economics) +
  scatterplot (mapping = aes(x = date, y = unemploy, size = uempmed))
ggplot(data = economics) +
  geom_point(mapping = aes(x = date, y = unemploy, size = uempmed))

# Exersice 4. Run the following code line by line and explain what it does.

economics$date
# Показывает все данныи из столбца date
# install.packages('lubridate')

library(lubridate)

year(economics$date)
# Выделяет года из всех дат
my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

# Exersice 5. Plot for x = uempmed, y = unemploy facet_wrap on decade.

ggplot(data = my_economics) + 
  geom_point(mapping = aes(x = uempmed, y = unemploy))+ facet_wrap(~decade)

# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.

ggplot(data = my_economics) +
  geom_smooth(mapping = aes(x = uempmed, y = unemploy))+ facet_wrap(~decade)

# Exersice 7. Plot histogram for unemploy for each decade.

ggplot(my_economics, aes(unemploy, fill = decade)) +
  geom_histogram()

# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.

ggplot(my_economics, aes(x = unemploy, y = decade)) +
  geom_boxplot() +
  coord_flip()

ggplot(my_economics) +
  stat_summary(
    mapping = aes(x = decade, y = unemploy),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
     )

# Exersice 9. Save one of the plots to directory img

ggsave ("img/22.pdf")

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct



