# Joba Adisa
# Lab 7

# Set up

## Loading libraries

library(tidyverse)
library(janitor)
library(readxl)
library(dataedu)
library(nlme)
library(emmeans)



# --------------------------- For my view -------------------------- #
buckets_raw <-  read_excel("data/BuCKETS_DATA.xlsx")


# ----------------------------------------------------------------- #
# Load and Process the data
buckets <-  read_excel("data/BuCKETS_DATA.xlsx")

# Clean data
buckets <- clean_names(buckets)

# Data Processing

buckets <- buckets %>% 
  # Dropping MSSA total Score
  select(!starts_with(c("wav", "fm", "eg", "mtr")), -science_courses, -age) %>%
  # converting the necessary columns to character type
  mutate_at(vars(id_number:type_of_hs), ~as.character(.)) %>% 
  # re-label the response options to align with the information provided in the code book for this data
  mutate(treatment = if_else(treatment == "1", "treatment", "control")) %>% 
  mutate(sex = if_else(sex == "1", "female", "male")) %>%
  mutate(race = case_when(black == "1" ~ "Black",
                          hispanic == "1" ~ "Hispanic",
                          TRUE ~ "White")) %>%
  
  mutate(year = case_when(year == "1" ~ "Freshman",
                          year == "2" ~ "Sophomore",
                          year == "3" ~ "Junior",
                          TRUE ~ "Senior")) %>%
  
  mutate(major = case_when(major == "0" ~ "Elementary Ed", 
                           major == "1" ~ "Secondary Ed", 
                           major == "2" ~ "Business", 
                           major == "3" ~ "Music Ed", 
                           major == "4" ~ "Accounting", 
                           major == "5" ~ "Mathematics", 
                           major == "6" ~ "Undeclared", 
                           TRUE ~ "Social Work")) %>%
  mutate(in_state = if_else(home_state == "MS", "yes", "no")) %>% 
  mutate(type_of_hs = if_else(type_of_hs == "0", "public", "private")) %>%
  select(id_number, treatment, sex, race, year, major, in_state, type_of_hs, 
         mssa_sum_pre:petas_s_sum_dp) %>%
  mutate(missing_dp = if_else(is.na(mssa_sum_dp), "yes", "no"))


# Longitudinal Analysis of MSSA Scores
# The longitudinal analysis will require that the data be in ‘long’ format with three rows per participant, one for each time point (pre, post, dp), with one column that records the time of the measurement and another column that records the MSSA score at that time point.

# pivoting data 
buckets_mssa <- buckets %>% pivot_longer(cols = starts_with("mssa"),
names_to = "time",
values_to = "mssa_score") %>% 
  select(!starts_with(c("pck", "petas"))) %>% 
  mutate(time = str_replace(time, "mssa_sum_", ""))


# Visualize Data

# Before fitting the model for MSSA scores, we will get a sense of the change over time 
# for both groups by creating a line plot of mean MSSA scores over 
# the three measurement time points separately for each group.


#line plot 
buckets_mssa %>% 
  group_by(treatment, time) %>% 
  summarize(mean_mssa = mean(mssa_score, na.rm = T)) %>% 
  ggplot(aes(x = time, y = mean_mssa, color = treatment)) + 
  geom_line(aes(group = treatment), size = 1) + 
  geom_point(pch = 19) + theme_dataedu() + 
  scale_x_discrete(limits = c("pre", "post", "dp"))

# Model Data
mssa_lme <- lme(mssa_score ~ treatment*time, 
                data = buckets_mssa, 
                random = ~time | id_number, 
                na.action = na.omit) 
anova(mssa_lme)


#follow-up confidence interval 
contrast(emmeans(mssa_lme, c("time", "treatment")), 'tukey')


# Exercise
# Carry out a longitudinal analysis of the PETAS-S scores by completing the following steps.


# Create a data frame where the PETAS-S scores have been pivoted to long format, with one column for time and another column for PETS-S scores.
# pivoting data 
buckets_petas_s <- buckets %>% pivot_longer(cols = starts_with("petas_s"),
                                         names_to = "time",
                                         values_to = "petas_s_score") %>% 
  select(!starts_with(c("pck", "mssa"))) %>% 
  mutate(time = str_replace(time, "petas_s_sum_", ""))



# Construct a line plot to visualize the change in mean PETAS-S score over time separately for each group (control and treatment).

#line plot 
buckets_petas_s %>% 
  group_by(treatment, time) %>% 
  summarize(mean_petas_s = mean(petas_s_score, na.rm = T)) %>% 
  ggplot(aes(x = time, y = mean_petas_s, color = treatment)) + 
  geom_line(aes(group = treatment), size = 1) + 
  geom_point(pch = 19) + theme_dataedu() + 
  scale_x_discrete(limits = c("pre", "post", "dp"))




# Fit a linear mixed effects model for the PETAS-S scores with a subject-specific random effect and fixed effects for time, treatment, and their interaction. Request an ANOVA table for the fitted model.
# Model Data
petas_s_lme <- lme(petas_s_score ~ treatment*time, 
                data = buckets_petas_s, 
                random = ~time | id_number, 
                na.action = na.omit) 
anova(petas_s_lme)


# We can remove the non-significant interaction term and 
# reassess the significance of the two main effects.
petas_s_lme.me <- lme(petas_s_score ~ treatment + time, 
                    data = buckets_petas_s, 
                    random = ~time | id_number, 
                    na.action = na.omit)

anova(petas_s_lme.me)

# 
# We see that the treatment effect is still not statistically significant, 
# so we can remove it and fit a model with time as the only effect.

petas_s_lme.time <- lme(petas_s_score ~ time, 
                      data = buckets_petas_s, 
                      random = ~time | id_number, 
                      na.action = na.omit)

anova(petas_s_lme.time)


# Now, we have a final model where only the main effect of time is statistically 
# significant. The contrast statement reveals that the delayed post mean is 
# significantly higher than both the pretest and posttest mean.
#follow-up confidence intervals 
contrast(emmeans(petas_s_lme.time, "time"), 'tukey')



# Note how the conclusion might differ if the correlation 
# between observations from the same individual 
# were ignored by fitting a simple linear regression model

#comparing results to linear model 
petas_s_lm <- lm(petas_s_score ~ time, data = buckets_petas_s) 

anova(petas_s_lm)
