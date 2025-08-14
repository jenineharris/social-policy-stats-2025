# STEP 1: TYPE IN DATA

# ADD SOME DATA
# the places of my life
# CHANGE THIS CODE TO YOUR PLACES
places <- c("Eugene, Oregon, USA",
            "Corvallis, Oregon, USA",
            "Avignon, France",
            "Washington DC, USA",
            "Bend, Oregon, USA",
            "St. Louis, Missouri, USA")

# the months I spent in each place (same order as places)
# CHANGE THIS CODE TO YOUR MONTHS
months <- c(302, 
            21, 
            18,
            12,
            12, 
            294) 

# make sure the number of PLACE entries is the same as the number of 
# entries in MONTHS

# STEP 2: COMBINE THE DATA INTO A DATA FRAME

# open the tidyverse package (install first through Tools menu)
library(package = "tidyverse")

# the bar plot needs a data frame rather than a table
# combine the vectors into a data frame
my_life_df <- data.frame(places, months)

# make sure the eras variable is in order
my_life_df$era <- factor(x = my_life_df$places, 
                         levels = my_life_df$places)


# STEP 3: MAKE THE GRAPH

# Try replacing "orange" with a color you like, like "red" or "blue" 
# or find a color code on the https://colorbrewer2.org/ website and it where 
# it currently says orange. 
# If you used a different measures like years or days, change the y label
# from "Number of months" to something else

# make the plot
my_life_df %>% 
  ggplot(aes(x = era, y = months)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Where I lived", 
       y = "Number of months")


# STEP 4 (optional): MAKE IT A WAFFLE! (buggy :-/)

# Waffle plots are a better alternative to a pie chart for showing 
# parts of a whole. Unfortunately, the waffle making
# R package is a little buggy, 
# so it might not work on your computer. 
# But, let's try it!
# The waffle package can only handle 8 categories maximum, 
# so if you have more eras than that, you can leave some out or combine them. 
# I combined the places I lived in more than one time.

# open the waffle package (install first through Tools menu)
library(package = "waffle")

# COPY AND PASTE YOUR PLACES VECTOR (REPLACE MINE) ON THE 
# RIGHT SIDE OF THE ARROW TO ASSIGN PLACES TO EACH OF THE
# MONTHS VALUES YOU ALREADY SPECIFIED ABOVE
names(months) <- c("Eugene, Oregon, USA",
                   "Corvallis, Oregon, USA",
                   "Avignon, France",
                   "Washington DC, USA",
                   "Bend, Oregon, USA",
                   "St. Louis, Missouri, USA")

# make the plot
waffle(months / 12, rows = 5,
       legend_pos = "bottom")
# once it runs, try replacing the 10 and 6 with different 
# numbers and see what happens
# the 12 divides the number of months by 12 so each square is worth 12
# months, the 5 is the number of rows in your waffle 
# try replacing "bottom" with "top" and see what happens


# RESOURCES FOR DOING MORE
# Bar graph options: https://ggplot2.tidyverse.org/reference/geom_bar.html
# Colors: https://colorbrewer2.org/