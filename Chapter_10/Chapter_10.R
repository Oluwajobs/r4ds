## Longitudinal Analysis with Students With Disability Data

library(tidyverse)
library(dataedu)
library(lubridate)
library(here)

# Getting files using the download.file() function

# 2012 Data

download.file(
  # the url argument takes a URL for a CSV file
  url =    'https://bit.ly/3dCtVtf',
  # destfile specifies where the file should be saved
  destfile =  here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2012.csv"),
  mode = "wb")

# 2013 data
download.file(
  url = 'https://bit.ly/33WXnFX',
  destfile = here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2013.csv"),
  mode ="wb")


# 2014 data
download.file(
  url = 'https://bit.ly/2UvSwbx',
  destfile = here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2014.csv"),
  mode = "wb")

# 2015 data
download.file(
  url =
    'https://bit.ly/39wQAUg',
  destfile = here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2015.csv"),
  mode = "wb")


# 2016 data
download.file(
  url = 'https://bit.ly/2JubWHC',
  destfile = here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2016.csv"),
  mode = "wb")

# 2017-18 data
download.file(
  url = 'https://bit.ly/2wPLu8w',
  destfile =  here::here("data",
    "longitudinal_data",
    "bchildcountandedenvironments2017-18.csv"),
  mode =  "wb")


# initial data exploration
read_csv(here::here("data", "longitudinal_data", "bchildcountandedenvironments2012.csv"))

# We need to skip some lines (1-4)
# initial data exploration
read_csv(here::here("data", "longitudinal_data", "bchildcountandedenvironments2012.csv"), skip = 4)  # Now that works


# list.files returns a vector of file names in the folder specified in the path argument.

# Get filenames from the data folder

filenames <- list.files(path = here::here("data", "longitudinal_data"), 
                        full.names = TRUE)

# Verify filenames is a list of filenames
filenames


# READING MULTIPLE FILES AT ONCE USING THE purr:map package

all_files <-  filenames %>% 
  # Apply the function read_csv to each element of the file names
  map(., ~read_csv(., skip = 4))

# Variables of first and second dataset don't match
identical(names(all_files[[1]]), names(all_files[[2]]))

# Variables of second and third dataset do match
identical(names(all_files[[2]]), names(all_files[[3]]))

# Checking the number of cols using map
all_files %>% 
  # apply map to each element
  map(ncol)


# combining the datasets at this stage results in the incorrect
# number of columns
bind_rows(all_files) %>%
  # check the number of columns
  ncol()

# Exploring 2016 dataset
## Look at the first 10 column names
names(all_files[[5]])[1:10]

# We should skip just 3 rows for the 2016 dataset to have the variable names
all_files[[5]] <-
  # Skip the first 3 lines instead of the first 4
  read_csv(filenames[[5]],  skip = 3)


## Picking Variables
# Combining select and contains makes selection easier once we know what we want
all_files[[1]] %>% 
  select(
    Year, 
    contains("State", ignore.case = FALSE),
    contains("SEA", ignore.case = FALSE),
    contains("male")
  )

# We will turn this code chunk into a function and apply it using map to all the list of files
pick_vars <-  function(df) {
  df %>% 
    select_at(vars(
      Year, 
      contains("State", ignore.case = FALSE),
      contains("SEA", ignore.case = FALSE),
      contains("male")
    ))
}

# use the function with `all_files`

all_files <- 
  all_files %>% 
  map(pick_vars)
  
  
  
# check variable names before using bind_row()
all_files %>% map(names)

# we see that each dataset’s variable namesare the same
# Now we can combine them

child_counts <- 
  all_files %>% 
  # combine all the datasets in `all_files`
  bind_rows()

# verifying the rows and cols in our dataset
str(child_counts)  # str shows the structure of an R object

# we can use count() to explore all the different disability groups in the dataset.Here’s the number of times
# an SEA Disability Category appears in the dataset:

# child_counts %>% count(`SEA Disability Category`)
# Since we will be visualizing and modeling gender variables for all students in
# the dataset, we’ll filter out allsubgroups except “All Disabilities” and the
# age totals:

child_counts <-  child_counts %>%
  filter(
  # filter all but the All Disabilities category
  `SEA Disability Category` == "All Disabilities" ,
  # filter all but the age totals
  `SEA Education Environment` %in%  c(
    "Total, Age 3-5", "Total, Age 6-21")
)


# renaming the variables

child_counts <- child_counts %>%
  rename(
  # change these columns to more convenient names
  year =  Year,
  state = "State Name",
  age =   "SEA Education Environment",
  disability = "SEA Disability Category",
  f_3_5 =  "Female Age 3 to 5",
  m_3_5 =  "Male Age 3 to 5",
  f_6_21 =  "Female Age 6 to 21",
  m_6_21 =  "Male Age 6 to 21"
  )

# Clean slate names
child_counts <- 
  child_counts %>% 
  mutate(state = tolower(state))


# Tidy the Data Set
# We can use pivot_longer() to bring the gender variable intoone column. In this
# transformation, we create two new columns: a gender column and a total column.

child_counts <- 
  child_counts %>% 
  pivot_longer(cols = f_3_5:m_6_21,
               names_to = "gender",
               values_to = "total")

# use `case_when()` to transform the values to either “f” or “m”:
child_counts <-  child_counts %>% 
  mutate(
    gender = case_when(
      gender == "f_3_5" ~ "f",
      gender == "m_3_5" ~ "m",
      gender == "f_6_21" ~ "f",
      gender == "m_6_21" ~ "m",
      TRUE ~ as.character(gender)
      )
    )

# changing the data type of total
child_counts <- child_counts %>%
  mutate(total = as.numeric(total))


# We also need to change the year column to a date type.
# The package {lubridate}has a handy function called ymd
# that can help us. We just have to use the truncated
# argument to let R know we don’t have a month and date to convert.

child_counts <- 
  child_counts %>% 
  mutate(year = ymd(year, truncated = 2))


#  Let’s use arrange() here to sort the dataset by the year, state, and gender columns
child_counts %>% 
  arrange(year, state, gender)


# We can simplify our dataset by removing the rows with NAs, leaving us with one
# row for each category:
child_counts <-  
  child_counts %>% 
  filter(!is.na(total))

# sorting the data again to verify
child_counts %>% 
  arrange(year, state, gender)


# Analysis

# --------- 1 ----------------------------------------------------------------#
# --------- end --------------------------------------------------------------#


# --------- 1 ----------------------------------------------------------------#
#' how have child counts changed over time?
#' # Use top_n() to know which states have the highest mean count

child_counts %>%
  group_by(state) %>%
  summarize(mean_count = mean(total)) %>%
  # which six states have the highest mean count of students with disabilities
  top_n(6, mean_count)


# Looking at the distribution of states with high count
high_count <- 
  child_counts %>% 
  filter(state %in% c("california", "florida", "new york", "pennsylvania", "texas"))

# Initial exploration

high_count %>% 
  filter(gender == "f", age == "Total, Age 6-21") %>%
  ggplot(aes(x = year,  y = total, color = state)) +
  geom_freqpoly(  stat =    "identity",
  size =  1) +  
  labs(  title =    "Count of Female Students in Special Education Over Time",
         subtitle = "Ages 6-21") +  
  scale_color_dataedu() +
  theme_dataedu()

# summarize that difference by looking at the median student 
# count for each state over the years:

high_count %>% 
  group_by(year, state) %>% 
  summarise(n = sum(total)) %>% 
  ggplot(aes(x = state, y =n)) +
  geom_boxplot(fill = dataedu_colors("yellow")) +
  labs(title = "Median Students with Disabilities Count",
         subtitle =  "All ages and genders, 2012-2017") +
  theme_dataedu()

# --------- end --------------------------------------------------------------#


# --------- 1 ----------------------------------------------------------------#
# Comparing the male to female ratio

high_count %>%
  group_by(year, state, gender) %>%
  summarize(total = sum(total)) %>%
  # Create new columns for male and female student counts
  pivot_wider(names_from = gender, values_from = total) %>%
  # Create a new ratio column
  mutate(ratio =  m  /  f) %>%
  ggplot(aes(x = year, y = ratio, color = state)) +
  geom_freqpoly(stat = "identity",  size =  1) +
  scale_y_continuous( limits = c (1.5, 2.5)) +
  labs(title = "Male Student to Female Student Ratio Over Time",
  subtitle = "Ages 6-21") +
  scale_color_dataedu() +
  theme_dataedu()
# --------- end --------------------------------------------------------------#



# --------- Analysis before Model ----------------------------------------------------------------#

#' Fitting a linear regression model that estimates the year as a predictor of the male to female ratio 
#' will help us do just that.

child_counts %>%
  filter(age  == "Total, Age 6-21") %>%
  pivot_wider(names_from = gender,
                values_from = total) %>% 
  ggplot(aes(x = f, y = m)) +
  geom_point(size = 3,  alpha = .5,
  color = dataedu_colors("green")) +
  geom_smooth() +
  labs(title = "Comparison of Female Students to Male Students in Special Education",
  subtitle = "Counts of students in each state, ages 6-21",
  x = "Female students",
  y = "Male students",
  caption = "Data: US Dept of Education") +
  theme_dataedu()

# Excluding the us and outlier and replotting male to female
child_counts %>% 
  filter(age  ==    "Total, Age 6-21") %>%
  pivot_wider(names_from = gender,
              values_from = total) %>%
  # Filter for female student counts less than 500,000
  filter(f <= 500000) %>%  
  ggplot(aes(x = f, y = m)) +
  geom_point( size = 3, alpha = .5, 
              color = dataedu_colors("green")) +
  labs(title = "Comparison of Female Students to Male Students with Disabilities",
  subtitle = "Counts of students in each state, ages 6-21.\n Does not include outlying area", 
  x = "Female students",
y = "Male students",
caption = "Data: US Dept of Education") +
theme_dataedu()


# --------- end --------------------------------------------------------------#


# --------- Model----------------------------------------------------#

# creating the model data
model_data <-
  child_counts %>%
  filter(age  == "Total, Age 6-21") %>%
  mutate(
    year = as.factor(year(year))) %>%
  pivot_wider(names_from = gender,
  values_from = total) %>%
  # Exclude outliers
  filter(f <= 500000) %>%
  # Compute male student to female student ratio
  mutate(ratio =  m / f) %>%
  select(-c(age, disability))

# checking the count of data per year
model_data %>%
  count(year)

# visualizing the ratio across all year
ggplot(data = model_data,
  aes(x = year, y = ratio)) +
  geom_jitter(alpha =  .5, 
              color = dataedu_colors("green")) +
  labs(title ="Male to Female Ratio Across Years (Jittered)") +
  theme_dataedu()


# Creating the model
ratio_year <-  
  lm(ratio  ~ year,  data =  model_data)

# summarizing the model
summary(ratio_year)

# if we use summary on our model data we can also see the value of the intercept
model_data %>% 
  filter(year  == "2012") %>%
  summary()


# verify this by looking at the median male to female ratio for each year:

model_data %>%
  group_by(year) %>%
  summarize(
  median_ratio =  median(ratio))

# Visualizing this as a box plot
model_data %>%
  pivot_longer(cols = c(f, m),
  names_to = "gender",
  values_to = "students") %>%
  ggplot(aes(x = year, y = students, color = gender)) +
  geom_boxplot() +
  scale_y_continuous(
    labels = scales::comma) +
  labs(title = "Median Male and Female Student Counts in Special Education",
  subtitle = "Ages 6-21. Does not include outlying areas and freely associated states",
  x = "",  
  y =  "",
  caption =  "Data: US Dept of Education") +
  scale_color_dataedu () +
  theme_dataedu()
