# BIVARIATE GRAPHS

# load the tidyverse
library(package = "tidyverse")
library(package = "janitor")
library(package = "lessR")

# import the General Social Survey data from 2022
download.file(url = "https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/2022_stata.zip", 
              destfile = "gss.zip")
unzip('gss.zip')
gss2022 <- Read("2022/GSS2022.dta") %>%
  clean_names()


# select and add 2 policy-related variables you are interested in 
# from pages 4 and 5 of codebook
# filter to keep only people working full time
# check the categories in the codebook for your policy-related variables and
# add any necessary data cleaning to the data management
gss2022.clean <- gss2022 %>% 
  select(age, sex, educ, wrkstat, hrs1, nateduc, natpark) %>% # add variables
  mutate_at(c('sex', 'educ', 'wrkstat', 'nateduc', 'natpark'), # add categorical variables
            sjlabelled::as_label) %>% 
  filter(wrkstat == "working full time") %>% 
  select(-wrkstat) %>% 
  mutate(educ = case_when(educ == "12th grade" ~ "High School",
                          educ %in% c("1 year of college",
                                      "2 years of college",
                                      "3 years of college") ~ "Some College",
                          educ %in% c("4 years of college",
                                      "5 years of college",
                                      "6 years of college",
                                      "7 years of college",
                                      "8 years of college") ~ "4 or More Years of College",
                          .default = "Less than High School"
                                      )) %>% 
  mutate(across(where(is.factor), ~if_else(. %in% c("skipped on web", 
                                                    "no answer",
                                                    "don't know",
                                                    "not applicable",
                                                    "iap"),
                                           NA_character_, .))) %>% 
  drop_na()

# TABLE (create a well-formatted table of the 6 variables in the clean data set)



# UNIVARIATE GRAPHS (uni for one, univariate is one variable)

# Graph of hours worked per week
workHours <- gss2022.clean %>%
  ggplot(aes(x = hrs1)) +
  geom_histogram() +
  labs(title = "Hours Worked per Week for Participants Who Work Full-Time", 
       subtitle = "General Social Survey 2022 (n = )") 
workHours

# Graph of age
ageHisto <- gss2022.clean %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  labs(title = "Age of Participants Who Work Full-Time", 
       subtitle = "General Social Survey 2022 (n = )") 
ageHisto

# Graph of sex
sexGraph <- gss2022.clean %>%
  ggplot(aes(x = sex)) +
  geom_bar() +
  labs(title = "Sex of Participants Who Work Full-Time", 
       subtitle = "General Social Survey 2022 (n = )") 
sexGraph

# Graph of educ policy
educPol <- gss2022.clean %>%
  ggplot(aes(x = nateduc)) +
  geom_bar() +
  labs(title = "Opinions about Spending on Nation's Education System", 
       subtitle = "General Social Survey 2022 (n = )") 
educPol

# Graph of parks policy
parkPol <- gss2022.clean %>%
  ggplot(aes(x = natpark)) +
  geom_bar() +
  labs(title = "Opinions about Spending on Nation's Parks", 
       subtitle = "General Social Survey 2022 (n = )") 
parkPol

# Graph of education
educGraph <- gss2022.clean %>%
  ggplot(aes(x = educ)) +
  geom_bar() +
  labs(title = "Educational Attainment of Participants Who Work Full-Time", 
       subtitle = "General Social Survey 2022 (n = )") 
educGraph

# go back to data management and fix any issues with the variables
# to put the educ categories in logical order, use the fct_relevel() 
# function


# BIVARIATE GRAPHS (bi is for two, bivariate have two variables)

# Two categorical variables

# The basics
educPolicyBySexBasic <- gss2022.clean %>%
  ggplot(aes(x = nateduc, fill = sex)) + # pick the one with fewer categories for fill
  geom_bar(position = "dodge") # position = "dodge" puts bars side-by-side
educPolicyBySexBasic

# Sex and education policy
educPolicyBySex <- gss2022.clean %>%
  ggplot(aes(x = nateduc, fill = sex)) +
  geom_bar(position = "dodge") + 
  labs(x = "Spending on Education System", fill = "Participant Sex",
       title = "Opinions about Spending on Education System by Sex", 
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")
educPolicyBySex

# Interpret the graph: 

# Sex and education with percentages
educPolicyBySexPerc <- gss2022.clean %>%
  ggplot(aes(x = nateduc, fill = sex,
             y = 100*(after_stat(count))/sum(after_stat(count)))) +
  geom_bar(position = "dodge") + 
  labs(x = "Spending on Education System", fill = "Participant Sex",
       y = "Percentage of ?",
       title = "Opinions about Spending on Education System by Sex", 
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")
educPolicyBySexPerc

# How are these percentages being computed? 



# Which bars add up to 100%?


# Sex and education with percentages
educPolicyBySexPerc2 <- gss2022.clean %>%
  group_by(nateduc, sex) %>% 
  count() %>% 
  group_by(nateduc) %>% # the variable you want to add up to 100%
  mutate(percentNat = 100*(n/sum(n))) %>% 
  ggplot(aes(x = nateduc, fill = sex,
             y = percentNat)) +
  geom_col(position = "dodge") +
  labs(x = "Spending on Education System", fill = "Participant Sex",
       y = "Percentage of ?",
       title = "Opinions about Spending on Education System by Sex", 
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")
educPolicyBySexPerc2

# How are these percentages being computed? 



# Which bars add up to 100%?




# Educ and education policy
educByEducPolicy <- gss2022.clean %>%
  group_by(educ, nateduc) %>% 
  count() %>% 
  group_by(nateduc) %>% # the variable you want to add up to 100%
  mutate(percentNat = 100*(n/sum(n))) %>% 
  ggplot(aes(fill = educ, x = nateduc, # group by variable on x-axis
             y = percentNat)) +
  geom_col(position = "dodge") + # position = "dodge" puts bars side-by-side
  labs(x = "Educational Attainment", fill = "Spending on Education",
       y = "Percentage of ?",
       title = "Educational Attainment and Educational Policy", 
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() 
educByEducPolicy

# What looks weird here? Go back to the data management and fix it.


# How are these percentages being computed? 


# Which bars add up to 100%?




# Educ and education policy
educByEducPolicy2 <- gss2022.clean %>%
  group_by(educ, nateduc) %>% 
  count() %>% 
  group_by(educ) %>% # the variable you want to add up to 100%
  mutate(percentEduc = 100*(n/sum(n))) %>% 
  ggplot(aes(x = educ, fill = nateduc,
             y = percentEduc)) +
  geom_col(position = "dodge") + 
  labs(fill = "Educational Attainment", x = "Spending on Education",
       y = "Percentage of ?",
       title = "Educational Attainment and Educational Policy", 
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() 
educByEducPolicy2


# Discuss the differences between the two ways of visualizing with percents: 



# Create a graph with one of your chosen policy variables and educ:




# ONE CATEGORICAL AND ONE CONTINUOUS

# Age & park spending policy graph, basic
ageEducPolicy <- gss2022.clean %>%
  ggplot(aes(x = age, y = natpark)) +
  geom_boxplot()
ageEducPolicy

# Same graph but make it fancy
ageEducPolicyFancy <- gss2022.clean %>%
  ggplot(aes(y = age, x = natpark)) +
  geom_boxplot(fill = "deeppink") +
  labs(y = "Age", x = "Spending on National Parks",
       title = "Opinions about Spending on National Parks by Age",
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() 
ageEducPolicyFancy

# Another option
ageEducPolicyHisto <- gss2022.clean %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 10, fill = "deeppink", color = "white") +
  facet_wrap(facets = vars(natpark)) +
  labs(x = "Age", y = "Frequency",
       title = "Opinions about Spending on National Parks by Age",
       subtitle = "General Social Survey 2022 (n = )") +
  theme_minimal() 
ageEducPolicyHisto

# Interpret the age graphs:


# TWO CONTINUOUS VARIABLES

# Age & hours worked per week, basic plot
ageHoursPerWk <- gss2022.clean %>%
  ggplot(aes(x = age, y = hrs1)) +
  geom_jitter() 
ageHoursPerWk

# Let's get fancy
ageHoursPerWk <- gss2022.clean %>%
  ggplot(aes(x = age, y = hrs1)) +
  geom_jitter(color = "navy", size = 3, alpha = .5) +
  theme_minimal()
ageHoursPerWk


# PICK TWO VARIABLES WE HAVE NOT GRAPHED TOGETHER AND CREATE YOUR OWN
# DATA VIZ


