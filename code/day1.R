# Set directy, load packages and clear workspace
setwd("~/repos/advent2018/data")
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, ggplot2, purrr, broom, readr)

# Read in data
day1_data <- read_csv("day1.txt", col_names = "var")

# Part 1: Simply need to sum
sum(day1_data$var)

# Part 2: Create all possible sums
initial_values <- day1_data$var
match <- F
i <- 1
while(match == F) {
  summed_data <- cumsum(rep(initial_values, i))
  if (max(duplicated(summed_data)) == 1) {
    match <- T
    print(summed_data[duplicated(summed_data)][1])
  }
  i <- i + 1
}

# Quicker version with manual upper bound
summed_data <- cumsum(rep(initial_values, 1000))
summed_data[duplicated(summed_data)][1]