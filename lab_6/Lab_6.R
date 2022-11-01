# Joba Adisa
# Lab 6

# Set up

## Loading libraries

library(tidyverse)
library(janitor)
library(readxl)
library(dataedu)
library(maps)
library(gridExtra)

## Creating a Map

counties <- map_data("county")
sc_counties <-  filter(counties, region == "south carolina")
view(sc_counties)

## ploting the map
ggplot(sc_counties, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkgrey", color = "black") +
  theme_void()  # removes the coordinate plane 


# Load and Process the data
school_data <-  read_excel("data/School Headcount by Gender, Ethnicity and Pupils in Poverty.xlsx", 
                           skip = 6, n_max = 1134)

view(school_data)


# cleaning the names using the janitor package
school_data <- clean_names(school_data)
names(school_data)

# Using a more convenient variable name
school_data <-  school_data %>% 
  select(x2, x4, white, yes) %>% 
  rename(district = x2, tot_student = x4, 
         wh_num = white, pov_num = yes)

view(school_data)


# Grouping the data by the district variable and using summarise to determine the total number of 
# BIPOC students and students in poverty across each county

county_data <- school_data %>% 
  mutate(district = str_replace_all(district, "[:digit:]", "")) %>% 
  mutate(district = tolower(trimws(district))) %>% 
  group_by(district) %>% 
  summarise(tot_student = sum(tot_student),
            wh_num = sum(wh_num), 
            pov_num = sum(pov_num)) %>% 
  mutate(wh_pct = wh_num / tot_student, bipoc_pct = 1 - wh_pct)

view(county_data)


# Exercise 1

#' Use mutate to create a new variable in the county_data dataframe 
#' for the percentage of students in poverty within each county
county_data <- county_data %>% 
  mutate(pov_pct = pov_num / tot_student)

# Joining the dataset
merged_data <-  left_join(sc_counties, county_data, by = c("subregion"="district"))
view(merged_data)


## Making a Choropleth Map

race_map <-  
  ggplot(merged_data, aes(x = long, y = lat, group = group, fill = bipoc_pct)) + 
  geom_polygon(colour = "black") + 
  scale_fill_gradient(low = "white", high = "black", labels = scales::percent) + 
  theme_void() + 
  theme(legend.position = c(0.9, 0.2)) + 
  labs(fill = "Percent BIPOC", title = "Percent of BIPOC Students by SC County") 

race_map


# Exercise 2

#'Create a choropleth map labeled pov_map for the percentage of students in poverty by SC county. 
#' Use the same white to black gradient for the scale fill.

pov_map <-  
  ggplot(merged_data, aes(x = long, y = lat, group = group, fill = pov_pct)) + 
  geom_polygon(colour = "black") + 
  scale_fill_gradient(low = "white", high = "black", labels = scales::percent) + 
  theme_void() + 
  theme(legend.position = c(0.9, 0.2)) + 
  labs(fill = "Percent in Poverty", title = "Percent of Students in Poverty by SC County") 

pov_map

grid.arrange(race_map, pov_map, nrow = 1)

#' The distribution of BIPOC and Students in Poverty in the maps above suggest that
#' Counties around the midlands and the Pee Dee area tend to have a high percentage of BIPOC Students
#' and also tended to have a high percentage of Students living in poverty.
#' Whereas


# Scatter plot with Labeled Points

#' The choropleth maps have illuminated some regional inequities that exist in South Carolina 
#' schools. Another way to visualize this data is to use a scatter plot with labeled points. 

ggplot(county_data, aes(x = wh_pct, y = pov_pct, label = district)) + 
  geom_text() + 
  labs(title = "Poverty Percentage vs. White Percentage Labeled by SC County",  
       x = "White Percentage", y = "Poverty Percentage") + 
  theme_dataedu() 


# We may also want to consider the correlation between percentage of white students and the 
# percentage of students in poverty by SC county. 

cor(county_data$wh_pct, county_data$pov_pct) 
# Thus, there is a fairly strong, negative correlation between the percentage of students that are 
# white and the percentage of students that are in poverty in a South Carolina county. 