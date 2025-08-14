# INSTRUCTIONS 

# Wherever you see a ________, replace it with whatever is needed for the code to work! 
# Find these blanks by looking for the red x next to the line number on the left. 
# The red x indicates something is 
# wrong with the code and is a great way to debug code that is not running as expected.


# if needed, install packages through the Tools menu: tidyverse, haven, table1, janitor
# fill in the blanks to load packages needed for this workshop
library(________ = "tidyverse")
library(package = ________)
________(package = "table1")
library(package = __________)


# IMPORTING THE DATA

# The Behavioral Risk Factor Surveillance Survey (BRFSS) data 
# can be found on the CDC website and downloaded directly into R from there
# The file is an xpt file (SAS transport file type) so the 
# import function is `read_xpt`

# 2023 Behavioral Risk Factor Surveillance Survey data
# opening directly from the online location
# this may take a few minutes depending on your computer and internet speed
temp <- tempfile(fileext = ".zip")
download.file(url  = "https://www.cdc.gov/brfss/annual_data/2023/files/LLCP2023XPT.zip",
              destfile = temp)
BRFSS_2023 <- read_xpt(file = temp) %>% 
  clean_names()

# Understanding the variables of interest

# We will answer the research question: 
# Are sex, age, race, and income associated with being denied insurance 
# coverage after cancer? 
    
# We will use the following variables:
    # sex
    # imprace
    # incomg1
    # age80
    # csrvdein
    
# Search the codebook to learn what these variables mean and how they were measured
# USCODE23_LLCP_021924 2.HTML
    
# List whether each variable is continuous or categorical and, 
# if categorical, what categories were used:

  # sex: 
  # imprace: 
  # incomg1: 
  # age80: 
  # csrvdein: 

# What are the descriptive statistics that would be appropriate 
# for each variable:

  # sex: 
  # imprace: 
  # incomg1: 
  # age80: 
  # csrvdein:  


# select variables
BRFSS_2023_small <- BRFSS_2023 %>%
   select(sex, ________, _______, ________, ________)

# summarize small data set
________(object = BRFSS_2015_small)


# BASED ON THE SUMMARY, WHAT IS A GOOD DATA MANAGEMENT PLAN?

# Recoding the sex variable 
# add on to the previous data management
  mutate(sex = recode_factor(________,
                             '2' = ________,
                             '1' = 'male'))

# Recoding the imprace variable
# add on to the previous data management
  mutate(imprace = recode_factor(________,
                                  '1' = 'White, non-Hispanic',
                                  '2' = 'Black, non-Hispanic',
                                  '3' = '________',
                                  '4' = 'American Indian/Alaskan Native, Non-Hispanic',
                                  '5' = '__________',
                                  '6' = '__________')) 

# YOU TRY IT!

# create the mutate code for incomg1 and csrvdein and add it to the code 
# above
  mutate(________ = recode_factor(________,
                                    '1' = ________,
                                    '2' = ________,
                                    '3' = ________,
                                    '4' = ________,
                                    '5' = ________,
                                    '6' = ________,                                
                                    '7' = ________,
                                    '9' = NA_character_)) %>% 
  mutate(________ = recode_factor(________,
                                    '1' = ________,
                                    '2' = ________,
                                    '7' = NA_character_,
                                    '9' = NA_character_)) 

# rename the variables and add on to existing data management
  rename(income = ________) %>% 
  rename(insuranceDenied = ________) %>% 
  rename(raceEth = ________) %>% 
  rename(ageYears = age80) %>% 
  drop_na()
# drop_na() with empty parentheses 
# drops any observation with an NA value for any variable

# CHOOSING APPROPRIATE DESCRIPTIVE STATISTICS

# check the continuous variable distributions
# plot the variable distribution to choose appropriate descriptive stats
ageHisto <- BRFSS_2023_small %>% 
  ggplot(aes(x = ________)) +
  geom_histogram()
ageHisto


# Describe the distribution and which descriptive statistics would be good:



# Make a table of descriptive statistics

# table of all variables
allVarsTable <- table1(~ sex + ageYears + income + ________ + ________,
       data = BRFSS_2023_small,
       caption = "")
allVarsTable

# Add labels to the table

# labels for each variable
label(BRFSS_2023_small$sex) = "Participant Sex"
label(_____________$ageYears) = ""
label(BRFSS_2023_small$income) = "Annual Household Income"
label(_____________$________) = ""
label(_____________$________) = "Participant Age in Years"

# Specify the appropriate descriptive statistics in the table
# table of all variables with labels
medianTable <- table1(~ sex + insuranceDenied + income + raceEth + ageYears, 
       render.continuous = c(. = "median (IQR)"),
       ________ = ________,
       caption = "")
medianTable

# Answer the research question

# table of gender identity and age by heavy drinker and sex
groupedTable <- table1(~ genderIdentity + ________ + ________ + sex | insuranceDenied, 
       render.continuous = c(. = "median (IQR)"),
       ________ = BRFSS_2023_small,
       caption = "")
groupedTable

# Interpret the descriptive statistics for the relationship between
# denied insurance and the three variables of interest:


# EXTRA STUFF (ON YOUR OWN): Examining relationships using graphs

# Insurance denied by sex
# graphing frequencies 
insurBySex <- BRFSS_2023_small %>% 
  ggplot(aes(x = ________, fill = sex)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Participant Denied Insurance", 
       fill = "Participant Sex") 
insurBySex

# graphing percentages (see page 48 in book & chapter 3) 
insurBySexPerc <- BRFSS_2023_small %>% 
  group_by(insuranceDenied, sex) %>% 
  count() %>% 
  group_by(insuranceDenied) %>% 
  mutate(perc.denied = 100*n/sum(n)) %>% 
  ggplot(aes(x = insuranceDenied, 
             y = perc.denied,
             fill = sex)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = ________, 
       y = ________, 
       fill = ________) 
insurBySexPerc

# Age by denied insurance
insurByAge <- BRFSS_2023_small %>% 
  ggplot(aes(x = ageYears, 
             fill = insuranceDenied)) +
  geom_histogram(color = 'white') +
  theme_minimal() +
  labs(x = ________, 
       y = ________, 
       fill = ________) 
insurByAge