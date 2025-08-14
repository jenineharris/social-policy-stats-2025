# REPRODUCING WORKMAN HISPANIC HEALTH AND DEMOGRAPHIC PAPER
# TABLE 1

# First we need to read through the methods in the paper to determine a few things: 

# Which data set was used in this study?

# What is the sample size shown in Table 1?

# Who is in the sample shown in Table 1? (everyone from the data set?)

# Which demographic characteristics are in Table 1?

# What are the variable names in the data set that correspond to the
# demographic characteristics in Table 1?
# codebook: https://www.cdc.gov/brfss/annual_data/2021/pdf/codebook21_llcp-v2-508.pdf

# How were the variables measured in the data set? (categorical, continuous,
# what categories included, etc.)

# How were the variables shown in Table 1? (categorical, continuous, 
# what categories included, etc.)

# What are the differences between how the data were measured and how
# they are displayed in the table?

# What data management is needed to get the data into shape for the Table?

# OPEN PACKAGES NEEDED
library(package = "tidyverse")
library(package = "janitor")
library(package = "table1")
library(package = "haven")

# IMPORT DATA
temp <- tempfile(fileext = ".zip")
download.file(url  = "https://www.cdc.gov/brfss/annual_data/2021/files/LLCP2021XPT.zip",
              destfile = temp)
BRFSS_2021 <- read_xpt(file = temp) %>% 
  clean_names()

# DATA MANAGEMENT

# Filter race/ethnicity for only Hispanic
# Several variables coupld be used for this including _RACEGR3
# which clean_names() changes to racegr3
# Hispanic is category 5
brfss_2021_clean <- BRFSS_2021 %>% 
  filter(racegr3 == 5) %>% 
# Filter for ages 45 and older
# can use multiple variables for this, try _AGE_G 
# since it already has the categories made
  filter(age_g > 3) #START HERE AND FINISH



# CREATE TABLE

# add arguments to get the table as close as possible
# to the published version
workmanTable <- table1(~ racegr3 + age_g + sexvar + 
                         chccopd3 + poorhlth + incomg1 +
                         smoker3 + asthma3 + hlthpln,
                      data = BRFSS_2021)
workmanTable

# SUMMARIZE YOUR FINDINGS

# Describe the differences between your table and the table in the Workman
# publication. What ideas do you have to explain the differences? What ideas do
# you have for your own work to ensure it can be reproduced?
