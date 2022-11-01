# 'Joba Adisa
# Lab 5

#--------------Setup---------------------------------------------#

# Load the tidyverse, visdat, and janitor packages.
library(tidyverse)
library(visdat)
library(janitor)

# Install and load openintro, plotly, GGally, ggmosaic, and ggridges packages.
# install.packages(c("openintro", "plotly", "GGally", "ggmosaic", "ggridges))

library(openintro)
library(plotly)
library(GGally)
library(ggmosaic)
library(ggridges)

#--------------Setup End---------------------------------------------#



#' ----------------Exploring Data--------------------------------------#
#' The data below comes from the openintro package and provides 
#' information about loans made through the Lending Club.
#' --------------------------------------------------------------------#

glimpse(loans_full_schema)

# consider a subset of this data by creating a smaller dataset named loans.
loans <- loans_full_schema %>% 
  select(loan_amount, interest_rate, term, grade, 
         state, annual_income, homeownership, debt_to_income)

# Using vis_dat to explore variable types and any missing data
vis_dat(loans)

#--------------------End----------------------------------------------#




#------------- Visualizing One Numeric Variable-----------------------#



#'------------------- Histogram---------------------------------------#
#'
ggplot(loans, aes(x = loan_amount)) +
  geom_histogram()


# Changing the bins of a histogram
# bin_width <-  1000
# The above histogram is too detailed to determine an overall trend.
ggplot(loans, aes(x = loan_amount)) +
  geom_histogram(binwidth = 1000)

# bin_width = 5000
# This histogram is more useful for determining the overall trend.
# Based on this histogram the distribution of loan_amount is unimodal and skewed to the right.
ggplot(loans, aes(x = loan_amount)) +
  geom_histogram(binwidth = 5000)

# bin_width = 20000
# Too much bin width and the graph begins to lack detail
ggplot(loans, aes(x = loan_amount)) + 
  geom_histogram(binwidth = 20000)

# Customizing histogram with labels
ggplot(loans, aes(x= loan_amount)) + 
  geom_histogram(binwidth = 5000) + 
  labs(x = "Loan Amount ($)", y = "Frequency", title = "Amount of Lending Club loans")


# Filling a histogram with a categorical variable
# We are breaking numerical variables by groups
ggplot(loans, aes(x = loan_amount, fill = homeownership)) +
  geom_histogram(binwidth = 5000, alpha=0.5) +
  labs(x = "Loan Amount ($)", y = "Frequency", title = "Amount of Lending Club loans")


# Facet function for adding a Categorical variable
# The facet function will provide distributions by groups in seperate stacks
ggplot(loans, aes(x = loan_amount, fill = homeownership)) +
  geom_histogram(binwidth = 5000, alpha=0.5) +
  labs(x = "Loan Amount ($)", y = "Frequency", 
       title = "Amount of Lending Club loans") +
  facet_wrap(~homeownership, nrow = 3)

# Facet function can also be used to group by two Categorical variables 
# using "+" operator
# The facet function will provide distributions by groups in seperate stacks
ggplot(loans, aes(x = loan_amount, fill = homeownership)) +
  geom_histogram(binwidth = 5000, alpha=0.5) +
  labs(x = "Loan Amount ($)", y = "Frequency", 
       title = "Amount of Lending Club loans") +
  facet_wrap(~ homeownership + grade, nrow = 3)

#---------------------------------------------------------------------------#


#---------------------------------------------------------------------------#
#                        Density Plot                                       #
#---------------------------------------------------------------------------#

# Density plots create a smoothed version of the histogram using a kernel density estimate.
ggplot(loans, aes(x = loan_amount)) +
  geom_density()

# Adjusting the bandwidth of Density Plots
# adjust = 0.5

loans %>% 
  ggplot(aes(x=loan_amount)) +
  geom_density(adjust = 0.5)

# adjust = 1

loans %>% 
  ggplot(aes(x=loan_amount)) +
  geom_density(adjust = 1)

# adjust = 2

loans %>% 
  ggplot(aes(x=loan_amount)) +
  geom_density(adjust = 2)

# Customizing Density plots with labels
loans %>% 
  ggplot(aes(x=loan_amount)) +
  geom_density(adjust = 2) + 
  labs(x="Loanamount($)", y = "Density", title = "Amounts of Lending Club loans")


# Adding a categorical variable to a Density Plot
# we can use fill, color, and shape
# trying fill
ggplot(loans, aes(x = loan_amount, fill = homeownership)) + 
  geom_density(adjust = 2, alpha = 0.5) + 
  labs(x = "Loan amount ($)", y = "Density", 
       title = "Amounts of Lending Club loans", fill = "Homeownership")


#------------------------------Boxplot-------------------------------------#

# Creating Boxplots
loans %>% 
  ggplot(aes(x=interest_rate)) +
  geom_boxplot()


# Customizing BoxPlots
ggplot(loans, aes(x=interest_rate)) + 
  geom_boxplot() +
  labs(x="Interest rate (%)", y=NULL, 
       title = "Interest rates of Lending Club loans") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Adding a categorical variable to the box plot

ggplot(loans, aes(x=interest_rate, y=grade, fill=grade)) + 
  geom_boxplot() +
  labs(x="Interest rate (%)", y="Grade", 
       title = "Interest rates of Lending Club loans", subtitle = "by grade of loan")

#--------------------------------End---------------------------------------#



#-----------------------------Summarizing One Numeric Variable-------------#
# note the difference between summary() and summarize()
loans %>% summarise(mean_loanAmt = mean(loan_amount))  # summarize can be used to collapse rows into summary stat.

# We can use varying aggregate statistics within the summarize function
# For example:

# Getting the mean, sd, min, and max of variables in the loans dataset
loans %>% 
summarise(mean_loanAmt = mean(loan_amount), sd_loanAmt = sd(loan_amount),
          min_loanAmt = min(loan_amount), max_loanAmt = max(loan_amount))


# We can also group a summary by the categorical variable in the data set
loans %>% 
  group_by(homeownership) %>% 
  summarise(mean_loanAmt = mean(loan_amount), sd_loanAmt = sd(loan_amount),
            min_loanAmt = min(loan_amount), max_loanAmt = max(loan_amount), n = n())

#'
#'
#'
#'----------------------------------------------------------------------------#
#'                                  Exercise 1
#'----------------------------------------------------------------------------#

loans %>% 
  ggplot(aes(x=interest_rate)) +
  geom_histogram( binwidth=2) +
  labs(x = "Interest Rate", y = "Frequency", title = "Interest rates of Lending Club loans")
  
# Filling a histogram with a categorical variable
  # We are breaking numerical variables by groups
  ggplot(loans, aes(x = interest_rate, fill = grade)) +
  geom_histogram(binwidth = 2, alpha=0.5) +
  labs(x = "Interest Rate", y = "Frequency", title = "Interest rates of Lending Club loans") +
    facet_wrap(~grade, nrow = 2)

# density plot
  ggplot(loans, aes(x = interest_rate, fill = grade)) + 
    geom_density(adjust = 2, alpha = 0.5) + 
    labs(x = "Interest Rate", y = "Density", 
         title = "Interest Rate of Lendee", fill = "grade")

  # Weird... they all look quite flat and undulating
  

  # create boxplots of interest_rate separated by grade, 
  # coloring each box a different color based on grade. 
  # Use appropriate labels and titles.
  
  ggplot(loans, aes(x=interest_rate, y=grade, fill=grade)) + 
    geom_boxplot() +
    labs(x="Interest rate (%)", y="Grade", 
         title = "Interest rates of Lending Club loans", 
         subtitle = "by grade of loan")
  


  # Data Summary
  loans %>% 
    group_by(grade) %>% 
    summarise(mean_interestAmt = mean(interest_rate), 
              sd_interestAmt = sd(interest_rate),
              min_interestAmt = min(interest_rate), 
              max_interestAmt = max(interest_rate), 
              iqr_Int = IQR(interest_rate), 
              n = n())
  




#'-------------------------------------End-----------------------------------#

#'
#'
#'
#'

#----------------- Visualizing two quantitative Variables--------------------#

# Scatterplot
ggplot(loans, aes(x=debt_to_income, y= interest_rate)) +
  geom_point()


# Hexplot
# When the scatter plot is too dense due to overlapping points
# The hexplot is can be useful
ggplot(loans, aes(x=debt_to_income, y= interest_rate)) +
  geom_hex()


# Hexplot using filter
# When the scatter plot is too dense due to overlapping points
# The hexplot is can be useful

# Creating hexplot for debt_to_income values less than 100
ggplot(loans %>% filter(debt_to_income < 100),
       aes(x=debt_to_income, y= interest_rate)) +
  geom_hex()


#'----------------------------------------------------------------------------#
#'                                  Exercise 2
#'----------------------------------------------------------------------------#

# Hexplot of loan_amount vs Interest_rate, filled by grade
loans %>%  
  ggplot() +
  geom_hex(aes(x=loan_amount, y= interest_rate, fill=grade)) +
  labs(title = "Hexplot of Loan Amount vs Interest Rate", x="Loan amount",
       y= "Interest rate", subtitle = "by grade of loan")



ggplot(loans %>% filter(grade %in% c("A", "B", "C", "D", "E", "F")),
       aes(x=loan_amount, y= interest_rate, fill=grade)) +
  geom_hex() +
  labs(title = "Hexplot of Loan Amount vs Interest Rate", x="Loan amount",
       y= "Interest rate", subtitle = "by grade of loan A - F")


#'-------------------------------------End-----------------------------------#




#----------------- Summarizing two quantitative Variables--------------------#
# Correlation
# the cor() function in base R returns a correlation matrix

loans.numeric <- loans %>% select(-grade, -state, -homeownership)

# The use="..." option computes the correlation of pairs of variables
cor(loans.numeric, use = "pairwise.complete.obs")  


# Correlation and Scatter plot Matrices in ggplot2

# ggpairs() - a function in GGally allows you to create scatterplots
loans.numeric %>% ggpairs(title = "Correlogram with ggpairs()") %>% ggplotly



#----------------------Visualizing Categorical Data---------------------------#
# Simple bar chart
ggplot(loans, aes(x = homeownership)) +
  geom_bar()

# Segmented Bar chart
ggplot(loans, aes(x = homeownership, fill = grade)) +
  geom_bar()

# this will produce the same result as the 
# This looks better to me
ggplot(loans, aes(x = homeownership, fill = grade)) +
  geom_bar(position = "fill")


# Customizing bar chart
ggplot(loans, aes(y = homeownership, fill = grade)) +
  geom_bar(position = "fill") + 
  labs(x= "Proportion", y="Homeownership", fill = "Grade",
       title = "Grades of Lending Club loans", subtitle = "and ownership of lendee")


# Mosaic Plot
loans <-  droplevels(loans)  # to drop unused levels of grades and homeownership

ggplot(loans) +
  geom_mosaic(aes(x = product(homeownership, fill = grade)))


#----------------------Summarizing Categorical Data---------------------------#

# Using the tabyl function in the janitor package to create 1, 2, and 3-way table
tabyl(loans, homeownership)  # creates a table showing summary of unique groups for the homeownership 

loans %>% 
  tabyl(homeownership, grade)



#----------------------Additional Relational Graphs for Numerical/Categorical Data---------------------------#

# Violin Plot
# A violin plot is similar to box plots with the addition of a rotated kernel density plot on each side.

ggplot(loans, aes(x = homeownership, y=loan_amount)) +
  geom_violin()


# Ridge Plot
# A ridgeline plot or joyplot shows the distribition of a numeir variable for several
# groups
loans %>% 
  ggplot() +
  geom_density_ridges(aes(x = loan_amount, y = grade, fill = grade, color=grade, alpha=0.5))
