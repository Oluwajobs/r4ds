print("We are going to use R as a calculator.")
print("First up, addition!")
12 + 8
632 + 41
print("Next, subtraction!")
48 - 6
0.65 - 1.42
#' This is an easy way to comment out a line
#' of code in R. 
#' By putting a ' sign in front of the pound (#) sign
#' You can keep commenting, forever!
#' Lol. Don't forget to delete the # sign on a new line
#' When you are done commenting.
#' Like this
print("Done commenting!")

# install {remotes}
install.packages("remotes", repos = "http://cran.us.r-project.org")
# install the {dataedu} package (requires R version 3.6 or higher)
remotes::install_github("data-edu/dataedu")
