# Set directy, load packages and clear workspace
setwd("~/repos/advent2018/data")
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, ggplot2, purrr, broom, readr, stringr)

# Read in data
data <- 
  read_csv("day2.txt", col_names = "var")

# Part 1: Split vector into characters then for each element of list, create flags if double or
# triple count exist.
# Then bind all rows of list, count total number of doubles and triples, and multiple
data_split <- str_split(data$var, "")
data_counted <- lapply(
  data_split, 
  function(x) {
    table(x) %>%
      as.data.frame() %>%
      count(Freq) %>%
      filter(Freq %in% c(2, 3)) %>%
      mutate(flag = 1) %>%
      select(Freq, flag)
  }
)

bind_rows(data_counted) %>%
  group_by(Freq) %>%
  summarize(count = sum(flag, na.rm = T)) %>%
  spread(key = Freq, value = count) %>%
  mutate(out = `2` * `3`)

# Part 2: Split vector into characters, then map characters to numbers based on position in 
# letters[] For each entry find difference of vector
data_split <- str_split(data$var, "")
data_mapped <- lapply(data_split, function(x) match(x, letters))

# For each entry find difference of vector with all remaining vectors in list and identify when 
# there are all zeros except 1. Then map matched numbers back to letters and print
for(i in 1:(length(data_mapped) - 1)) {
  for(j in (i + 1):length(data_mapped)) {
    test <- data_mapped[[i]] - data_mapped[[j]] == 0
    if(sum(test) == length(data_mapped[[i]]) - 1) {
      common <- data_mapped[[i]][test]
      common_letters <- letters[common]
      print(paste(common_letters, collapse = ""))
    }
  }
}