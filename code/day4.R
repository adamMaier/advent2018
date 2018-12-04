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
starts_sleep <- p1_data %>%
  filter(id == p1_id & action == "falls asleep") %>%
  pull(minute)

ends_sleep <- p1_data %>%
  filter(id == p1_id & action == "wakes up") %>%
  pull(minute)

result <- rep(0, 60)
for (i in 1:length(starts_sleep)) {
  new_vec <- c(
    rep(0, starts_sleep[i] - 1), 
    rep(1, ends_sleep[i] - starts_sleep[i]),
    rep(0, 1 + 60 - ends_sleep[i])
  )
  result <- result + new_vec
}
p1_minute <- which(result == max(result))

p1_minute * as.numeric(p1_id)