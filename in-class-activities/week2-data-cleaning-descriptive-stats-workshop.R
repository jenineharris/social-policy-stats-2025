# HOW TO FOLLOW ALONG AND PARTICIPATE

# Install the `tidyverse`, `table1`, and `haven` packages (if you haven't already)
# Use the `Tools` menu and choose the `Install packages...` menu choice 
# Type the names of the packages, `tidyverse, table1, haven`, in the dialog box 
# that opens and click Install

# RUN THE LIBRARY CODE TO OPEN THE PACKAGES NEEDED FOR TODAY
library(package = "tidyverse")
library(package = "haven")
library(package = "table1")
library(package = "janitor")
library(package = "sjlabelled")


# IMPORTING THE DATA INTO R

# use read_spss function to import the spss file 
globalData2019 <- read_sav(file = "Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav") %>% 
  clean_names()


# SELECTING VARIABLES AND OBSERVATIONS

# filter is for rows / observations
# select is for columns / variables
globalData2019clean <- globalData2019 %>%
  filter(country %in% c(3, 5, 7, 9, 11, 26, 16, 34, 33, 19)) %>% 
  select(country, sex, age, better_gender, country_satis, 
         policy_climate, policy_tariff, policy_immig, global_community) 


# CHECK YOUR WORK
summary(object = globalData2019clean)

# CHANGING DATA TYPES
# country should be a factor data type
# use as_label to change it to a factor
# choose a set of countries you are interested in
globalData2019clean <- globalData2019 %>%
  select(country, sex, age, better_gender, country_satis, 
         policy_climate, policy_tariff, policy_immig, global_community) %>% 
  mutate(country = as_label(country)) %>% 
  filter(country %in% c("Brazil", "Canada", "France", "Greece",
                        "India", "South Africa", "Kenya", "Tunisia", 
                        "Ukraine", "United States")) 


# CHANGING DATA TYPES: Sex variable
# add to the code above: mutate(sex = as_label(sex))

# CHANGING DATA TYPES: Better_gender & Country_satis
# add to the code above: 
# mutate(better_gender = as_label(better_gender))
# mutate(country_satis = as_label(country_satis))

# CHANGING DATA TYPES: You Try It!
# Add recoding of the rest of the variables in the smaller data set 
# to the code above and run the code
# Run the summary() code above to check your work


# EFFICIENT CODING

# When you use the same function in the same way 3 times or more, 
# look for a different solution
# In this case, we are using the as_label() function 6 times
# This works fine but is more code than we need and more code means the potential for more errors
# Try one of the other mutate functions to handle multiple
# variables at once, like mutate_at() 

globalData2019clean <- globalData2019 %>%
  select(country, sex, age, better_gender, country_satis, 
         policy_climate, policy_tariff, policy_immig, global_community) %>% 
  mutate_at(c("country", "sex", "better_gender", 
              "country_satis", "policy_climate", "policy_tariff",
              "policy_immig", "global_community"), as_label) %>% 
  filter(country %in% c("Brazil", "Canada", "France", "Greece",
                        "India", "South Africa", "Kenya", "Tunisia", 
                        "Ukraine", "United States")) 


# RECODING TO SPECIFY MISSING VALUES

globalData2019clean <- globalData2019 %>%
  select(country, sex, age, better_gender, country_satis, 
         policy_climate, policy_tariff, policy_immig, global_community) %>% 
  mutate_at(c("country", "sex", "better_gender", 
              "country_satis", "policy_climate", "policy_tariff",
              "policy_immig", "global_community"), as_label) %>% 
  filter(country %in% c("Brazil", "Canada", "France", "Greece",
                        "India", "South Africa", "Kenya", "Tunisia", 
                        "Ukraine", "United States")) %>% 
  set_na(na = c("Don’t know (DO NOT READ)", 
                "Refused (DO NOT READ)"))


# MAKE A BASIC TABLE

# We can start with a simple table with the `table1` package that 
# shows the `sex`, `age`, and `better_gender` variables with 
# their descriptive statistics

basicTable <- table1(~ sex + age + better_gender,
       data = globalData2019clean)
basicTable

# ADD LABELS FOR A BETTER TABLE

# Add labels for the variables and add the `policy_climate` variable
# to the table

label(globalData2019clean$sex) <- "Sex"
label(globalData2019clean$age) <- "Age (years)" 
label(globalData2019clean$better_gender) <- "Who has a better life in this country"
label(globalData2019clean$policy_climate) <- "US withdrawing from climate change agreements"

labelTable <- table1(~ sex + age + better_gender + policy_climate,
       data = globalData2019clean)
labelTable

## Basic table with labels and grouped by sex

groupedTable <- table1(~ age + better_gender + policy_climate | sex,
       data = globalData2019clean)
groupedTable

# DESCRIPTIVE STATISTICS FOR CONTINUOUS VARIABLES

# Choosing descriptive statistics for categorical variables 
# does not require extra testing, it is always frequency and percentage
# Choosing descriptive statistics for continuous variables requires 
# examining the distribution of the variable. For normally distributed 
# continuous variables, the mean and standard distribution are good 
# representations of the middle of the data and are commonly used. 
# For non-normally distributed continuous variables, the median and 
# interquartile range are usually the best option.
  
# CHECKING THE DISTRIBUTION OF AGE

# histogram of age variable
ageHisto <- globalData2019clean %>% 
  ggplot(aes(x = age)) +
  geom_histogram()
ageHisto

# MODIFY THE TABLE TO SHOW MEDIAN AND IQR

medianTable <- table1(~ age + better_gender + policy_climate | sex,
       data = globalData2019clean,
       render.continuous = "Median, IQR")
medianTable

# REMOVE MISSING VALUES FROM THE TABLE

# add render.missing = NULL inside the medianTable code

# YOU TRY IT! 

# Add the rest of the variable names to the table code from the 
# no-missing-values-table.
# Make sure to make better labels first.

label(globalData2019clean$country_satis) = ""
label(globalData2019clean$policy_immig) = ""
label(globalData2019clean$policy_tariff) = ""
label(globalData2019clean$global_community) = ""

noMissTable <- table1(~ age + better_gender + policy_climate | sex,
       data = globalData2019clean,
       render.continuous = "Median, IQR",
       render.missing = NULL)
noMissTable

# EXTRA: Modifying factor labels manually 
# (we will do this again later if we run out of time, feel free to try on your own)
# A few of the category labels could be modified, 
# like "Who has a better life in this country" has a "Both equally" 
# response and the label that came from the data source says 
# "Both equally (DO NOT READ)" for the data collectors which will be 
# confusing for an audience

# Mutate can be used to modify category labels, here is an example of 
# modifying the better_gender variable

globalData2019clean <- globalData2019 %>%
  filter(country %in% c(3, 5, 7, 9, 11, 26, 16, 34, 33, 19)) %>% 
  select(country, sex, age, better_gender, country_satis, 
         econ_sit, believe_god, global_community) %>% 
  mutate_at(c("country", "sex", "better_gender", 
              "country_satis", "econ_sit", "believe_god",
              "global_community"), as_label) %>% 
  set_na(na = c("Don’t know (DO NOT READ)", 
                "Refused (DO NOT READ)")) %>% 
  mutate(better_gender = recode_factor(better_gender,
                                       "Both equally (DO NOT READ)" = "Both equally"))

# Try adding code to modify the labels for global_community
# Then run the new code and re-run the noMissTable code
# to see how it looks!
  
## The end


