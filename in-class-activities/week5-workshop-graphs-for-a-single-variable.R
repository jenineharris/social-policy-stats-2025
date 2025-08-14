# GOAL: Use the 2024 National Public Opinion Reference Survey (NPORS) to review 
# code and concepts from Chapters 1-3.
# 
# RESEARCH GOAL: Exploratory analysis for a study that examines whether age, 
# gender, political party, and educational attainment help to explain opinions 
# on gun ownership.

# ACTIVITIES:
# 
# 1) Import data
# 2) Examine imported data 
# 3) Use the codebook to learn about the variables (https://www.pewresearch.org/wp-content/uploads/sites/20/2024/07/2024-NPORS-Online-Questionnaire.pdf)
# 4) Select the variables of interest into a smaller data frame
# 5) Recode the variables so that "don't know" and "refused" are treated as missing 
# 6) Drop any observations that are missing data on any of the 3 variables of interest 
# 7) Add labels to factor variables and do any other cleaning needed
# 8) Visualize each variable with a well-formatted plot


# open the packages needed to import, manage, and organize the data
library(package = "tidyverse")
library(package = "haven")
library(package = "table1")
library(package = "janitor")
library(package = "sjlabelled")
library(package = "cowplot")

# import data 
npors2024 <- read_sav(file = "NPORS_2024_for_public_release.sav") %>% 
  clean_names()

# Examine imported data
# this code is an alternative to clicking on the name of the data
# in the environment window
View(x = npors2024)

# Find each of the following variables in the codebook and indicate the 
# meaning of the variable and how it was measured for each one. Use age as a 
# model for the others.
# 
# 1) age
# Question: What is your age? 
# Responses: 998 = Don't know, 999 = Refused, or an age in years
# Data type: continuous 
# 
# 2) educcat
# Question: 
# Responses:
# Data type: 
# 
# 3) gender 
# Question: 
# Responses:
# Data type: 
# 
# 4) party
# Data type: 
# 
# 5) moregunimpact 
# Question: 
# Responses:
# Data type: 


# select and manage variables
# run the code below and examine the data 
# fill in the things that could be coded as NA and add pipe to run
# codebook: https://www.pewresearch.org/wp-content/uploads/sites/20/2024/07/2024-NPORS-Online-Questionnaire.pdf
npors2024cleaned <- npors2024 %>% 
  select(age, educcat, moregunimpact, gender, party) %>% 
  mutate_at(c("educcat", "gender", "party", 
              "moregunimpact"), as_label)  
  set_na(na = c("", "", "", ""))

# Describe the purpose of each of the functions used so far:
# 
# * The library() function was used to...
# * The read_sav() function was used to...
# * The clean_names() function was used to...
# * The View() function was used to...
# * The select() function was used to...
# * The as_label() function was used to...
# * The set_na() function was used to...

# summarize the managed data set
summary(object = npors2024cleaned)

# Choose the appropriate descriptive statistics for the continuous variable by 
# examining the data distribution. 

npors2024cleaned %>% 
  ggplot(aes(x = age)) +
  geom_histogram()

# make a table of the managed data set
# the dot is used to signify that all variables in the data
# should be in the table

# make good labels for the table first by filling in the quote marks
label(educcat) = ""
label(moregunimpact) = ""
label(gender) = ""
label(party) = ""

# choose the appropriate descriptive stats for the age variable and 
# add inside the quote marks
table1(~ .,
       data = npors2024cleaned,
       render.continuous = "")

# examine how age, education, gender, and party are distributed by gun impact
# choose the appropriate descriptive stats for the age variable and 
# add inside the quote marks
table1(~ age + educcat + gender + party | moregunimpact,
       data = npors2024cleaned,
       render.continuous = "Mean, SD")

# ***The categories of gun impact seem confusing to anyone who hasn't 
# read the codebook, which would typically be your entire audience. 
# What does "More crime" mean? Would it be more clear to say, 
# "More gun ownership = more crime" instead? Go back to the data management 
# code and change the categories to better reflect the question and answers. 
# Then, rerun the table code.***

# Write bullet points interpreting at least 2 of the descriptive statistics from 
# each of the tables:
# 
# * 
# *
# *
# * 


# Add the appropriate geom layer to the code to choose an appropriate graph type 
# for each of the variables. Different data types will have different types of 
# graphs (e.g., bar graph, histogram, density plot).

# Add a layer with a title and better labels for the x and y axes to each graph.
# 
# Make any other formatting changes you'd like (e.g., add a theme, add color).
# 
# Interpret what you see in each graph in one sentence. 


# visualize age
ageGraph <- npors2024cleaned %>% 
  ggplot(aes(x = age)) +
  # add an appropriate geom here 
ageGraph 
  
# INTERPRETATION:

# visualize age again!
# pick a different way to visualize age from your first graph
ageGraph2 <- npors2024cleaned %>% 
  ggplot(aes(x = age)) +
  # add an appropriate geom here 
ageGraph2

# INTERPRETATION:


# visualize moregunimpact
gunImpactGraph <- npors2024cleaned %>% 
  ggplot(aes(x = moregunimpact)) +
  # add an appropriate geom here
gunImpactGraph

# INTERPRETATION: 


# visualize educcat 
educGraph <- npors2024cleaned %>% 
  ggplot(aes(x = educcat)) +
  # add an appropriate geom here
educGraph

# INTERPRETATION: 


# visualize gender 




# INTERPRETATION: 


# visualize political party 




# INTERPRETATION: 



# BIVARIATE GRAPHS

# Follow section 3.6.3.4 to write new R code to make a ****boxplot**** of the age 
# and gun impact variables together. Age is continuous and gun impact 
# is categorical.

# Follow section 3.6.2 in the book to write new R code to make a 
# ****grouped bar graph**** of the party and gun impact 
# variables together. Party and gun impact are both categorical.



# PRINT THE SINGLE VARIABLE GRAPHS AS A GROUP

# The cowplot package allows you to group plots into a single data
# visualization. Try creating a group plot of four of the single 
# variable plots you created. 
# Put the names of the plots in the code with commas between the plot
# names, like the ageGraph that is already there

plot_grid(ageGraph, , , ,
          labels = c('A', 'B', 'C', 'D'),
          label_size = 12)


# IMPROVE THE GRAPHS

# For the bar graphs you created, go back to the code and 
# change the statistic shown on the y-axis to a percentage rather than a 
# frequency (see code for figures 3.15 and 3.16 in the Harris textbook 
# for examples). Change the interpretation to match the updated graph.

# Re-run each plot and the plot_grid code. Fix anything that needs fixing.



