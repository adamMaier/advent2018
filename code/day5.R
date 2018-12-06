# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, readr, stringr, lubridate)

# Read in data
data <- 
  read_delim(
  "~/repos/advent2018/data/day5.txt", 
    col_names = "var", 
    col_types = "c", 
    delim = ";;;"
  ) %>%
  pull(var)



# PART 1 -------------------------------------------------------------------------------------------

# Create all combinations of letters to remove
to_drop <- paste(c(paste0(letters, LETTERS), paste0(LETTERS, letters)), collapse = "|")

# Loop through string until length no longer changes. Writing as a function to use later
init_length <- 0
final_length <- 1
react_fn <- function(x) {
  while(init_length != final_length){
    init_length <- nchar(x)
    x <- str_remove_all(x, to_drop)
    final_length <- nchar(x)
  }
  nchar(x)
}
react_fn(data)



# PART 2 -------------------------------------------------------------------------------------------

# Repeat same while loop as above, storing the final number for each letter.
results <- c()
for(i in letters){
  letter_out <- paste(i, toupper(i), sep = "|")
  remaining <- str_remove_all(data, letter_out)
  results[match(i, letters)] <- react_fn(remaining)
}
min(results)