# Set directy, load packages and clear workspace
setwd("~/repos/advent2018/data")
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, ggplot2, purrr, broom, readr, stringr)

# Read in data
data <- read_delim("day3.txt", col_names = "var", col_types = "c", delim = ";;;")

# Part 1: Extract key dimensions of grid as separate variables, then split data into list
data %<>%
  mutate(
    h_edge = str_extract(var, "(?<=@ )(.+)(?=,)"),
    v_edge = str_extract(var, "(?<=,)(.+)(?=:)"),
    width = str_extract(var, "(?<=: )(.+)(?=x)"),
    height = str_extract(var, "(?<=x)(.+)")
  ) %>%
  mutate_at(vars(h_edge:height), as.numeric)

data_list <- split(select(data, -var), 1:nrow(data))

# Begin with empty matric and loop through each list element and add 1 for element corresponding to 
# grid.
tapestry <- matrix(data = 0, nrow = 1000, ncol = 1000)
for(i in 1:length(data_list)) {
  tapestry[
    (data_list[[i]]$v_edge + 1):(data_list[[i]]$v_edge + data_list[[i]]$height), 
    (data_list[[i]]$h_edge + 1):(data_list[[i]]$h_edge + data_list[[i]]$width)
  ] <- 1 +
    tapestry[
      (data_list[[i]]$v_edge + 1):(data_list[[i]]$v_edge + data_list[[i]]$height), 
      (data_list[[i]]$h_edge + 1):(data_list[[i]]$h_edge + data_list[[i]]$width)
    ]
}
sum(tapestry >= 2)

# Part 2 - Using tapestry from before, for each matrix in list of data, see if difference contains
# no value greater than 1
for(i in 1:length(data_list)) {
  total <- 
    sum(
      tapestry[
        (data_list[[i]]$v_edge + 1):(data_list[[i]]$v_edge + data_list[[i]]$height), 
        (data_list[[i]]$h_edge + 1):(data_list[[i]]$h_edge + data_list[[i]]$width)
      ] >= 2
    )
  if(total == 0) {
    print(data[i, ])
  }
}