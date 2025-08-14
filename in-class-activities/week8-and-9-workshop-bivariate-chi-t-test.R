## Purpose

# Use chi-squared and t-tests to determine which characteristics are associated with being a zombie. 
# Practice the chi-squared and t-test process and interpretation.

# import the zombie data
# open needed packages 
# make sure you have tidyverse, table1, haven, descr, and labelled packages
library(package = "tidyverse")
library(package = "table1")
library(package = "labelled")
library(package = "haven")
library(package = "descr")
# import data
zombies <- read_dta("zombies.dta")


## We need a codebook
codebook.zombies <- labelled::generate_dictionary(data = zombies)
codebook.zombies


## Clean data

zombies <- zombies %>% 
  mutate(zombie = recode_factor(.x = zombie,
                                '0' = "Not zombie",
                                '1' = "Zombie")) %>% 
  mutate(waterPerPerson = water / household) %>% 
  mutate_if(is.character, as.factor)

## Chi-squared 

# Chi-squared is used to determine...
# 
# In determining the characteristics associated with being a zombie, 
# which is a categorical variable, which of the variables below would be 
# examined using chi-squared (delete the ones that are not):
# 
# * **gender:** sex (female/male)
# * **rurality:** lives in rural/suburban/urban area
# * **radio:** have a battery powered radio (yes/no)
# * **flashlight:** have a flashlight (yes/no)
# * **water:** how many gallons of water are available
# * **firstaid:** have a first aid kit (yes/no)
# * **waterPerPerson:** gallons of water per person in the household
# * **age:** age in years
# * **household:** how many people live in the household
# * **food:** there is enough food for a week (yes/no)

## What are the assumptions for chi-squared?



## Conducting chi-squared

## Make a table to explore the data first

zombieTable <- table1(~ gender + rurality + radio + 
                        flashlight + firstaid + age | zombie,
       data = zombies)


# Based just on these descriptive statistics, where does it 
# look like there may be a relationship between zombie status and 
# some characteristic? 

## Sex and zombie status

# H0: 
# 
# HA: 

## Make a prediction about what you will find based on data visualization 

# sex & zombie status (see page 289)
sexZombieStatus <- zombies %>% 
  group_by(gender, zombie) %>% 
  count() %>% 
  group_by(gender) %>% 
  mutate(perc.gender = 100*n/sum(n)) %>% 
  ggplot(aes(x = gender, y = perc.gender, fill = zombie)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "Percent of zombies within gender group",
       x = "Gender")
sexZombieStatus

# Prediction: 

# Conduct chi-square analysis


# sex and zombie status
CrossTable(y = zombies$zombie,
           x = zombies$gender,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)

# Does it meet assumptions and why or why not?: 



# Interpret the results: 



### Rurality and zombie status

# H0: 
# 
# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 


# rural & zombie status (see page 289)
ruralZombieStatus <- zombies %>% 
  group_by(rurality, zombie) %>% 
  count() %>% 
  group_by(rurality) %>% 
  mutate(perc.rurality = 100*n/sum(n)) %>% 
  ggplot(aes(x = rurality, y = perc.rurality, fill = zombie)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "Percent of zombies within rurality group",
       x = "Rurality")
ruralZombieStatus

# Prediction: 

#### Conduct the analysis

#  rurality and zombie status
CrossTable(y = zombies$zombie,
           x = zombies$rurality,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)

# Does it meet assumptions and why or why not?: 



# Interpret the results:



### Radio and zombie status

# H0: 
# 
# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 

# radio & zombie status (see page 289)
radioZombieStatus <- zombies %>% 
  group_by(radio, zombie) %>% 
  count() %>% 
  group_by(radio) %>% 
  mutate(perc.radio = 100*n/sum(n)) %>% 
  ggplot(aes(x = radio, y = perc.radio, fill = zombie)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "Percent of zombies within radio group",
       x = "Has radio")
radioZombieStatus

# Prediction: 

#### Conduct the analysis

# radio and zombie status
CrossTable(y = zombies$zombie,
           x = zombies$radio,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)


# Does it meet assumptions and why or why not?: 
 
# Interpret the results:


### Flashlight and zombie status

# H0: 
# 
# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 


# flashlight & zombie status (see page 289)
flashlightZombieStatus <- zombies %>% 
  group_by(flashlight, zombie) %>% 
  count() %>% 
  group_by(flashlight) %>% 
  mutate(perc.flashlight = 100*n/sum(n)) %>% 
  ggplot(aes(x = flashlight, y = perc.flashlight, fill = zombie)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "Percent of zombies within flashlight group",
       x = "Has flashlight")
flashlightZombieStatus

# Prediction: 

#### Conduct the analysis


# flashlight and zombie status
CrossTable(y = zombies$zombie,
           x = zombies$flashlight,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)


# Does it meet assumptions and why or why not?: 


# Interpret the results:

### First aid kit and zombie status

# H0: 

# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 


# first aid kit & zombie status 



# Prediction: 

#### Conduct the analysis


# first aid kit and zombie status


# Does it meet assumptions and why or why not?: 


# Interpret the results:


############################################################t-tests

## Independent samples t-tests

# Independent samples t-tests are used to...
# 
# In determining the characteristics associated with being a zombie, which 
# is a categorical variable, which of the variables in this data set would be 
# examined using a t-test (delete the ones that are not):

# * **gender:** sex (female/male)
# * **rurality:** lives in rural/suburban/urban area
# * **radio:** have a battery powered radio (yes/no)
# * **flashlight:** have a flashlight (yes/no)
# * **water:** how many gallons of water are available
# * **firstaid:** have a first aid kit (yes/no)
# * **waterPerPerson:** gallons of water per person in the household
# * **age:** age in years
# * **household:** how many people live in the household
# * **food:** there is enough food for a week (yes/no)

## What are the assumptions for an independent samples t-test?


## Conducting t-tests

### Age and zombie status

# H0: 

# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 


# age & zombie status (see page 346)
zombies %>% 
  ggplot(aes(x = age, fill = zombie)) +
  geom_density(alpha = .8) +
  theme_minimal() +
  labs(x = "Distribution of age",
       y = "Probability density")

# Prediction: 

#### Conduct the analysis


# age and zombie status
t.test(formula = zombies$age ~ zombies$zombie,
       var.equal = TRUE)


# Does it meet assumptions and why or why not?: 




# distribution
ageDist <- zombies %>% 
  ggplot(aes(x = age)) +
  geom_density() +
  facet_grid(cols = vars(zombie)) +
  theme_minimal()
ageDist

# equal variances 
car::leveneTest(y = age ~ zombie, data = zombies)

# Interpret the results: 


# mann-whitney U
wilcox.test(formula = zombies$age ~ zombies$zombie)



# [disclaimer: since we didn't meet assumptions, we'd actually do a mann-whitney U test instead
# and there is no need to do the t-test first!]

### Water per person and zombie status

# H0: 
# 
# HA: 

#### Make a prediction about what you will find based on exploratory analysis: 

# water per person & zombie status (see page 346)
waterByZombie <- zombies %>% 
  ggplot(aes(y = waterPerPerson, x = zombie)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Zombie status",
       y = "Gallons of water per person")
waterByZombie

# Prediction: 


# water per person and zombie status
t.test(formula = zombies$waterPerPerson ~ zombies$zombie,
       var.equal = TRUE)


# Does it meet assumptions and why or why not?: 




# distribution
waterDist <- zombies %>% 
  ggplot(aes(x = waterPerPerson)) +
  geom_density() +
  facet_grid(cols = vars(zombie)) +
  theme_minimal()
waterDist


#equal variances
car::leveneTest(y = waterPerPerson ~ zombie, data = zombies)



# Interpret the results:


# [disclaimer: we did not meet assumptions so would usually choose a 
# different analysis strategy; categorize water per person and use chi-squared]

## Overall interpretation

# What is associated with being a zombie? 


## If there is time...a code shortcut 


library(package = "tableone")

# create table for zombie data
# with zombie (outcome variable) as the strata
# table now shows p-values for bivariate tests
# chi-squared for categorical+categorical
# t-test for continuous+binary
# Mann-Whitney U test for non-normal continuous+binary
zombieTable <- CreateTableOne(data = zombies, 
                              vars = c("age", "gender",
                                       "rurality", "food",
                                       "radio", "flashlight",
                                       "firstaid", "waterPerPerson"),
                              strata = "zombie")
print(zombieTable, showAllLevels = TRUE,
      nonnormal = c("age", "waterPerPerson"))



## The End