# Book: R for Data Science https://r4ds.had.co.nz
# Chapters 1-4 

# Some RStudio’s keyboard shortcuts:
# 
#  Alt + - (the minus sign) -  <- (assignment sign)
# Ctrl + Shift + C – comment/uncomment
# ?command or help(command) - help
# Ctrl + Enter - run selected lines (see menu Code)
# Ctrl + L – clear console

# theme Cobalt: Tools - Glbal Options - Appearance - Cobalt - Apply

# View - Panes - Show all panes


# Visualization----------------------------------------------------------------



# 1. First steps---------------------------------------------------------------

# install.packages("ggplot2")

library(ggplot2)
?ggplot2

# Install a package once, reload it every time you start a new session.

# Check in Packages, what libraries are installed (their versions) and loaded.

# package::function() - to be explicit about where a function (or dataset) comes from.
# e.g. ggplot2::ggplot() When to use this?

View(mpg)
mpg

str(mpg)

mpg$model

mpg$model[1:10]

unique(mpg$model)

# How to get help about mpg?

?mpg

# Creating a scatterplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Check Plots pane.

# What does the plot show?

# ggplot(data = mpg) creates an empty graph.
# geom_points() adds a layer of points to your plot.
# Mapping argument is always paired with aes().


# x and y of aes() specify which variables to map to the x and y axes.

# Set working directory and save plot to it.
# More info about save() https://ggplot2.tidyverse.org/reference/ggsave.html 

getwd()
setwd("D:/R projects/ds-courses/BSU/visualization")

ggsave("mpg.pdf")
ggsave("mpg.png")


# 2. Aesthetic mappings--------------------------------------------------------

# How to visualize three variables on one plot?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Does it explains outliers?

# Two other ways
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))



# 3. Facets--------------------------------------------------------------------

# To facet by a single variable use facet_wrap().
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# To facet by two variables use facet_grid(). 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)



# 4. Geometric objects---------------------------------------------------------

# Which geom have we already used? (geom_point)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Not every aesthetic works with every geom, e.g. shape of a point and linetype of a line.

# Use linetype to add drv to the previous plot.
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
 
# More geoms - ggplot2 cheatsheet at https://www.rstudio.com/resources/cheatsheets/

# Multiple geoms in the same plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))



# 5. Statistical transformations-----------------------------------------------
# geom_bar()
# https://ggplot2.tidyverse.org/reference/geom_bar.html
ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class))

# The same as previous written in a short form
ggplot(mpg, aes(class)) + 
  geom_bar()

df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
ggplot(df, aes(x)) + geom_bar()

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill = drv))

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill = drv), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")  

# geom_histigram()
# https://ggplot2.tidyverse.org/reference/geom_histogram.html
# bins = 30 by default
ggplot(mpg, aes(hwy)) +
  geom_histogram()

ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 5)

ggplot(mpg, aes(hwy)) +
  geom_histogram(bins = 40)

ggplot(mpg, aes(hwy, fill = class)) +
  geom_histogram()


# stat_summary() 
ggplot(mpg) + 
  stat_summary(
    mapping = aes(x = class, y = cty),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# geom_boxplot() & coord_flip()
ggplot(mpg, aes(x = class, y = cty)) + 
  geom_boxplot() +
  coord_flip()


# 5. Sum up--------------------------------------------------------------------
# Let's have a look through the code and sum up what we have learned.
