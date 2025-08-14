# packages for the project
library(package = "tidyverse") 
library(package = "forcats")
library(package = "table1")
library(package = "tableone")
library(package = "gghalves")
library(package = "janitor")

# import the data
covid <- read.csv(file = "COVID-19 Vaccine Survey Responses.csv")

#take out international - does not align with research question
covid.dirty <- covid %>% 
  select( Once.it.is.available..do.you.intend.to.get.the.vaccine.for.COVID.19.,
          Have.you.received.a.flu.shot.in.the.past.12.months., 
          What.is.the.highest.degree.or.level.of.school.you.have.completed., 
          Which.most.closely.aligns.with.your.political.beliefs.,
          I.am.at.risk.for.contracting.COVID.19.,
          COVID.19.can.be.a.serious.disease.,
          The.COVID.19.vaccine.will.prevent.me.from.contracting.COVID.19., 
          There.are.many.risks.associated.with.getting.the.COVID.19.vaccine.,
          State.of.Residence,
          What.is.your.age.in.years.) %>% 
  mutate(State.of.Residence = recode_factor(.x = State.of.Residence,
                                            `International` = NA_character_)) %>% 
  filter(!is.na(State.of.Residence))

#Recode and label variables, ensure missing values are properly coded
covid.small <- covid.dirty %>% 
  select( Once.it.is.available..do.you.intend.to.get.the.vaccine.for.COVID.19.,
          Have.you.received.a.flu.shot.in.the.past.12.months., 
          Which.most.closely.aligns.with.your.political.beliefs.,
          What.is.your.age.in.years.) %>% 
  #mutate_all(as.factor) %>% 
  rename(vaccine.intention = Once.it.is.available..do.you.intend.to.get.the.vaccine.for.COVID.19.) %>% 
  rename(flu.shot.history = Have.you.received.a.flu.shot.in.the.past.12.months.) %>%
  mutate(flu.shot.history = recode_factor(.x = flu.shot.history,
                                          `Don't know/Not sure` = NA_character_)) %>% 
  rename(political.beliefs = Which.most.closely.aligns.with.your.political.beliefs.) %>% 
  mutate(political.beliefs = recode_factor(.x = political.beliefs,
                                           '.' = "Prefer not to answer")) %>% 
  rename(age.years = What.is.your.age.in.years.) %>% 
  mutate(age.years = as.numeric(as.character(age.years))) %>% 
  drop_na()


#summarize the cleaned data 
summary(covid.small)

#Creating levels for each variable so they are ordered in logical sense on graphs below
covid.small$flu.shot.history <- factor(covid.small$flu.shot.history,
                                       levels = c("Yes", "No"))
covid.small$vaccine.intention <- factor(covid.small$vaccine.intention,
                                        levels = c("Yes", "No", 
                                                   "Don't Know/Not sure"))
covid.small$political.beliefs <- factor(covid.small$political.beliefs, 
                                        levels = c("Very Conservative", 
                                                   "Somewhat Conservative", 
                                                   "Mixed", 
                                                   "Somewhat Liberal", 
                                                   "Very Liberal", 
                                                   "Prefer not to answer"))

# check age distribution
hist(covid.small$age.years)
# age distribution is right skewed, use medians

#Creating table to show descriptive statistics in percents 
#add labels to the table
labelled::var_label(x = covid.small) <- c("Intends to get the vaccine for COVID-19",
                                          "Flu shot in the past 12 months",
                                          "Political beliefs",
                                          "Age in years (median, IQR)")

# use print to show table with labels and percent
table1 <- table1(~ vaccine.intention + flu.shot.history +
         political.beliefs + age.years,
       data = covid.small,
       render.continuous = "Median, IQR",
       caption = "Characteristics of 511 Participants in a Covid-19 Vaccination Survey, 2022.")

# hide this but use info for bullet points below
table1(~ flu.shot.history + 
         political.beliefs + age.years | vaccine.intention,
       data = covid.small,
       render.continuous = "Median, (Q1, Q3)")

covid.small <- covid.small %>% 
  mutate(vaccine.intention = recode_factor(vaccine.intention,
                                           "Don't Know/Not sure" = "Unsure Intent to get Covid Vaccine",
                                           "No" = "No Intent to get Covid Vaccine (No)",
                                           "Yes" = "Intent to get Covid Vaccine (Yes)",
  ))

#Figure 1 - Flu Shot History Graphed with Percent in Intention to get COVID-19 Vaccine
figure1 <- covid.small %>% 
  drop_na(vaccine.intention, flu.shot.history) %>% 
  group_by(vaccine.intention, flu.shot.history) %>% 
  count() %>% 
  group_by(vaccine.intention) %>% 
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot(aes(x = flu.shot.history, fill = vaccine.intention,
             y = percent)) +
  geom_col(position = "dodge") +
  theme_bw(base_size = 18, base_family = 'serif') + 
  theme(strip.background = element_rect(fill = "#eff3ff")) +
  labs(x = "Have you received a flu shot\nin the past 12 months?", 
       y = "Percent in vaccine intention group",
       title = "Figure 1: Vaccine Intention & Flu Shot History\n(2022; n = 511)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"), 
                    guide = "none") +
  facet_wrap(facets = 'vaccine.intention', nrow = 3) +
  coord_flip()
figure1

# chi-squared flu shot history and vax intention
descr::CrossTable(x = covid.small$flu.shot.history,
                  y = covid.small$vaccine.intention,
                  sresid = TRUE,
                  expected = TRUE,
                  prop.r = FALSE,
                  prop.c = FALSE,
                  prop.t = FALSE,
                  chisq = TRUE)
# assumptions met, expected values are large in all cells

#Figure 3 - Political Beliefs Graphed with Percent in Intention to get COVID-19 Vaccine
figure3 <- covid.small %>% 
  drop_na(vaccine.intention, political.beliefs) %>% 
  group_by(vaccine.intention, political.beliefs) %>% 
  count() %>% 
  group_by(vaccine.intention) %>% 
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot(aes(x = political.beliefs, fill = vaccine.intention,
             y = percent)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw(base_size = 18, base_family = 'serif') + 
  theme(strip.background = element_rect(fill = "#eff3ff")) + 
  labs(x = "Political beliefs", 
       y = "Percent in vaccine intention group",
       title = "Figure 2: Vaccine Intention &\nPolitical Beliefs (2022; n = 511)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                    guide = "none") +
  facet_wrap(facets = 'vaccine.intention', nrow = 3)
figure3

descr::CrossTable(x = covid.small$political.beliefs,
                  y = covid.small$vaccine.intention,
                  sresid = TRUE,
                  expected = TRUE,
                  prop.r = FALSE,
                  prop.c = FALSE,
                  prop.t = FALSE,
                  chisq = TRUE)
# assumptions are met, expected values are large in all cells

figure4 <- covid.small %>% 
  ggplot(aes(fill = fct_rev(vaccine.intention),
             y = age.years,
             x = vaccine.intention)) +
  geom_half_violin(aes(fill = fct_rev(vaccine.intention)), alpha = .4, side = "r") +
  geom_half_point(aes(color = fct_rev(vaccine.intention)), alpha = .8, side = "l") +
  theme_bw(base_size = 18, base_family = 'serif') + 
  labs(y = "Age in Years", 
       x = "Intent to Get Covid Vaccine",
       title = "Figure 3: Vaccine Intention & Age in Years\n(2022; n = 511)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                    guide = "none") +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                     guide = "none") +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 15),
    drop = FALSE
  ) +
  coord_flip()


# fails the assumption of normal distribution within groups, the Yes group is very right skewed
# the don't know group is somewhat right skewed
# Kruskal-Wallis test
kruskal.test(formula = age.years ~ vaccine.intention,
             data = covid.small)
# stat sig so do follow-up test
dunn.test::dunn.test(x = covid.small$age.years,
                     g = covid.small$vaccine.intention,
                     method = "bonferroni")

# POLICY BRIEF

# data on current covid vaccination intent by state overall
stateCovidVaxIntent2025 <- read_csv("Weekly_Cumulative_COVID-19_Vaccination_Coverage_and_Intent__Overall__by_Selected_Demographics_and_Jurisdiction__Among_Adults_18_Years_and_Older_20250408.csv") %>% 
  clean_names() %>% 
  filter(geographic_level == "State" & 
           demographic_level == "Overall" &
           covid_season == "2024-2025" &
           month_week == "December Week 1") %>% 
  select(geographic_name, indicator_category_label, estimate) %>% 
  pivot_wider(names_from = indicator_category_label, 
              values_from = estimate) %>% 
  clean_names() %>% 
  rename(state_name = geographic_name) %>% 
  select(state_name, definitely_or_probably_will_not_get_a_vaccine) %>% 
  mutate(definitely_or_probably_will_get_a_vaccine = 
           1-definitely_or_probably_will_not_get_a_vaccine/100)

# make a map
library(package = 'urbnmapr')
library(package = 'urbnthemes')
library(package = 'scales')

intentionMapCovid <- stateCovidVaxIntent2025 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, 
                       fill = definitely_or_probably_will_get_a_vaccine)) +
  geom_polygon(color = "#ffffff", size = .25) +
  urbnthemes::scale_fill_gradientn(labels = scales::percent,
                                   guide = guide_colorbar(title.position = "top"),
                                   limits = c(.3, .8),
                                   breaks = c(.4, .5, .6, .7)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Intent to Get Covid Vaccine") +
  theme_void(base_size = 24) +
  theme(legend.position = "top",
        legend.key.width = unit(3, "null"))
intentionMapCovid

# data on current flu vaccination by state overall
stateFluVax2025 <- read_csv("Weekly_Influenza_Vaccination_Coverage_and_Intent_for_Vaccination__Overall__by_Selected_Demographics_and_Jurisdiction__Among_Adults_18_Years_and_Older_20250408.csv") %>% 
  clean_names() %>% 
  filter(geographic_level == "State" &
         demographic_level == "Overall" & 
           influenza_season == "2024-2025" &
         month_week == "February Week 2") %>% 
  select(geographic_name, indicator_category_label, estimates) %>% 
  pivot_wider(names_from = indicator_category_label, 
              values_from = estimates) %>% 
  clean_names() %>% 
  rename(state_name = geographic_name) %>% 
  select(state_name, definitely_or_probably_will_not_get_a_vaccine) %>% 
  mutate(definitely_or_probably_will_get_a_vaccine = 1-
           definitely_or_probably_will_not_get_a_vaccine/100)

vaxMapFlu <- stateFluVax2025 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, 
                       fill = definitely_or_probably_will_get_a_vaccine )) +
  geom_polygon(color = "#ffffff", size = .25) +
  urbnthemes::scale_fill_gradientn(labels = scales::percent,
                                   guide = guide_colorbar(title.position = "top"),
                                   limits = c(.3,.8),
                                   breaks = c(.4,.5, .6, .7)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Intent to Get Flu Vaccine") +
  theme_void(base_size = 24) +
  theme(legend.position = "top",
                       legend.key.width = unit(3, "null"))
vaxMapFlu

# Revise Figure 1 
library(waffle)

# Vector
x <- data.frame(group = c("No COVID-19 Vaccination Intent",
                          "Yes or Maybe COVID-19 Vaccination"),
                value = c(11, 89))

# Waffle chart
waffleplot <- ggplot(x, aes(fill = group, values = value)) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "white") +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "",
       title = "100 People with a Flu Vaccine") +
  coord_equal() +
  theme_void(base_size = 24) +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(reverse=FALSE, ncol = 1))
waffleplot
