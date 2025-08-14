# R You Ready? An Introduction to R
# Week 1 Workshop

# How to follow along and participate
# Install the tidyverse, table1, and haven packages 
# Use the Tools menu and choose the Install packages... menu choice 
# Type the names of the packages, tidyverse, table1, haven, in the dialog box 
# that opens and click Install
# highlight the next lines of code and click Run or use keyboard shortcut to run
# on a mac it is ctrl-enter, on a PC it is alt-enter
library(package = "tidyverse")
library(package = "table1")
library(package = "haven")
library(package = "sjlabelled")
library(package = "janitor")

# Importing data into R

# use read_spss function to read the spss file 
globalAttitudes2023 <- read_sav(file = "Pew Research Center Global Attitudes Spring 2023 Dataset.sav") %>% 
  clean_names()

# Choosing variables

# use select to choose variables
globalAttitudes2023Rec <- globalAttitudes2023 %>% 
  select(country, age, sex, abortion_legal, gay_marriage)

# Check your work (use after each step to make sure data look good!)

# use summary to see summary statistics
# on all the data in your data set
summary(object = globalAttitudes2023Rec)

# Choosing variables & observations

# select function to select variables (columns)
# filter function to select observations (rows)
globalAttitudes2023Rec <- globalAttitudes2023 %>% 
  select(country, age, sex, abortion_legal, gay_marriage) %>% 
  filter(age > 25)


# Managing data types in R

# Commonly used data types in R:
# factor: variable with categories (categorical data) 
# numeric: variable with values over some continuum (continuous data) 
# integer: whole number with values over some continuum (often treated as continuous)
# character: used for words or other verbatim information (e.g., ZIP codes, names) 
    
# Several of the variables are the wrong data type! Which ones?
    
# The `mutate()` function is useful for changing variable types or values or really anything 
# We can add this function into our data management chain

# Recoding variables

# recode age and sex
globalAttitudes2023Rec <- globalAttitudes2023 %>% 
  select(country, age, sex, abortion_legal, gay_marriage) %>% 
  filter(age > 25) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(sex = as_label(sex))

# Recoding more variables

# recode abortion_legal by adding the next line to the code above
# mutate(abortion_legal = as_label(abortion_legal))


# recode country by adding the next line to the code above
# mutate(country = as_label(country))


# correcting the age variable
# Check the codebook to see that the values of 98 and 99 are
# "Don't know" and "Refused" and do not represent people who 
# are actually 98 and 99 years old
# Use the replace() function to replace these values with NA, 
# which is "not available" or missing in R
# recode age by adding the next line to the code above
# mutate(age = replace(age, age > 97, NA)) 

# You Try It!

# Finish the recoding yourself by adding the 
# gay_marriage variable to the data cleaning


## Basic table

# basic table with 4 variables
basicTable <- table1(~ sex + abortion_legal + 
                       gay_marriage + age,
       data = globalAttitudes2023Rec)
basicTable

## Fixing labels and units

label(globalAttitudes2023Rec$sex) <- "Participant sex"
label(globalAttitudes2023Rec$age) <- "Age in years"
label(globalAttitudes2023Rec$abortion_legal) <- "Abortion should be"
label(globalAttitudes2023Rec$gay_marriage) <- "Allow gay marriage"

units(globalAttitudes2023Rec$age) <- "years"

## Fancier table

fancyTable <- table1(~ sex + abortion_legal + 
                       gay_marriage + age,
       data = globalAttitudes2023Rec,
       render.continuous = c(. = "Mean (SD)"),
       caption = "Characteristics and Attitudes of Global Survey Participants, 2023 (n = 21,672)")
fancyTable

## Stratified table

stratTable <- table1(~ abortion_legal + 
                       gay_marriage + age | sex,
       data = globalAttitudes2023Rec,
       render.continuous = c(. = "Mean (SD)"),
       caption = "Characteristics and Attitudes of Global Survey Participants by Sex, 2023 (n = 21,672)")
stratTable

## Basic bar chart

barChart <- 
  globalAttitudes2023Rec %>%
  ggplot(aes(x = gay_marriage)) +
  geom_bar() 
barChart

## The end
