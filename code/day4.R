# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, ggplot2, purrr, broom, readr, stringr, lubridate)

# Read in data
data <- read_delim(
  "~/repos/advent2018/data/day4.txt", 
  col_names = "var", 
  col_types = "c", 
  delim = ";;;"
)


# WRITE FUNCTIONS ----------------------------------------------------------------------------------

# Write a function to create a vector given start and end sleep times
create_vec <- function(start = NULL, end = NULL) {
  if (start != 0) {
    c(rep(0, start - 1), rep(1, end - start), rep(0, 1 + 60 - end))
  } else {
    c(rep(1, end - 1), rep(0, 1 + 60 - end))
  }
}

# Write a function to extract start and end times from a chronological order of sellp then wake then
# slepp then... wake
time_extract <- function(data = NULL) {
  
  starts_sleep <- data %>%
    filter(action == "falls asleep") %>%
    pull(minute)
  
  ends_sleep <- data %>%
    filter(action == "wakes up") %>%
    pull(minute)
  
  out_vec <- rep(0, 60)
  for (i in 1:length(starts_sleep)) {
    new_vec <- create_vec(start = starts_sleep[i], end = ends_sleep[i])
    out_vec <- out_vec + new_vec
  }
  out_vec
}


# PART 1 -------------------------------------------------------------------------------------------

# Extract key dimensions of data as separate variables, round any time prior to midnight to midnight,
# then order chronologically
p1_data <- data %>%
  separate(var, into = c("date_time", "action"), sep = "] ") %>%
  mutate(date_time = ymd_hm(str_sub(date_time, 2))) %>%
  arrange(date_time)

# Apply guard ID to all associated rows of shift 
p1_data %<>%
  mutate(id = parse_number(action)) %>%
  fill(id)

# Calculate number of minutes slept in each interval
p1_data %<>%
  filter(action %in% c("falls asleep", "wakes up")) %>%
  mutate(
    minute = minute(date_time),
    sleep_time = ifelse(
      action == "wakes up",
      minute - lag(minute),
      NA
    )
  )

# Find id with most combined sleep minutes
p1_id <- p1_data %>%
  group_by(id) %>%
  summarize(total_st = sum(sleep_time, na.rm = T)) %>%
  ungroup() %>%
  filter(total_st == max(total_st)) %>%
  pull(id)

# Find the minute most slept by keeping records of id and converting them to vectors of 60, then
# summing
result <- time_extract(p1_data %>% filter(id == p1_id))
p1_minute <- which(result == max(result))

# Print result
p1_minute * as.numeric(p1_id)



# PART 2 -------------------------------------------------------------------------------------------

# Using prepped p1 data, group by ID and apply function to all IDs
p2_data <- split(p1_data, f = p1_data$id)

# For each ID, identify the max number of overlapped minutes, then the max of the max
p2_data <- 
  lapply(p2_data, function(x) {
    vec <- time_extract(x)
    out_minute <- which(vec == max(vec))
    out_total <- max(vec)
    c(out_total, out_minute)
    }
  )

# Identify individual with most frequent minute slept
p2_id <-
  lapply(p2_data, function(x) x[1]) %>%
  unlist()

p2_id <- names(p2_id[which(p2_id == max(p2_id))])

# Multiplying result
as.numeric(p2_id) * p2_data[[p2_id]][2]