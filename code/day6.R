# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, readr, stringr, lubridate)

# Read in data
data <- read_csv("~/repos/advent2018/data/day6.txt", col_names = c("x", "y"))
x <- pull(data, x)
y <- pull(data, y)


# PART 1 -------------------------------------------------------------------------------------------

# Adjust all coordinates so minimum edge is 1
x1 <- x - min(min(x), min(y)) + 1
y1 <- y - min(min(x), min(y)) + 1

# Create 2-column matrix with every coordinate between 1,1 and max of adjusted y and x
full_points <- 
  matrix(
    c(rep(1:max(x1, y1), each = max(x1, y1)), rep(1:max(x1, y1), times = max(x1, y1))), 
    ncol = 2
  )

# For each point in matrix, calclate the manhattan distance with all provided points
min_dist <- c()
for(i in 1:nrow(full_points)) {
  min_vals <- dist(rbind(full_points[i, ], cbind(x1, y1)), method = "manhattan")[1:length(x1)]
  min_pos <- which(min_vals == min(min_vals))
  if (length(min_pos) == 1) {
    min_dist[i] <- min_pos
  } else {
    min_dist[i] <- NA
  }
}

# Identify the row numbers that have a minimum coordinates corresponding to an edge
edges <-
  which(
    full_points[, 1] == min(full_points[, 1]) | 
    full_points[, 2] == min(full_points[, 2])
  )

id_drop <- unique(min_dist[edges])

# Drop Ids that are infinite from full set of values, then take max of tabled values
max(table(min_dist[!min_dist %in% id_drop]))