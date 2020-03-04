# View dataset "economics" from ggplot2. Read help on this dataset. 
# For each variable see meaning, type and values. 

# Exersice 1. For any of 5 timeseries (pce, pop, psavert, uempmed, unemploy) make plots (x = date) using geoms point, line and smooth.

# Exersice 2. Display pce and psavert on one plot.

# Exersice 3. Display three variables on one scatterplot (x = date, y = unemploy, size = uempmed).

# Exersice 4. Run the following code line by line and explain what it does.

economics$date

year(economics$date)

my_economics <- economics
my_economics$decade <- year(economics$date) %/% 10 * 10

class(my_economics$decade)

my_economics$decade <- as.factor(my_economics$decade)

class(my_economics$decade)

# Exersice 5. Plot for x = uempmed, y = unemploy facet_wrap on decade.

# Exersice 6. On one plot (x = uempmed, y = unemploy) display a separate geom_smoth object for each decade.

# Exersice 7. Plot histogram for unemploy for each decade.

# Exersice 8. Plot geom_boxplot and stat_summary for unemploy for each decade.

# Exersice 9. Set working directory and save one of the plots to it

# Exersice 10. Compose 3 questions (in Russian and English) on the topic 'visualisation' with 3-4 variants of answers only 1 of wich is correct



