# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, readr, stringr, lubridate, purrr)

# Read in data
data <- 
  read_delim("~/repos/advent2018/data/day8.txt", delim = ";", col_names = c("v")) %>%
  pull(v) %>%
  str_split(" ", simplify = T)
             


# PART 1 -------------------------------------------------------------------------------------------

p1 <- as.numeric(data)

# If number of child nodes is not 0, skip to the next node, using i and j to track node location
# and location of next available space.
# i is node position; j is next available position
nodes <- meta <-  rep(F, length(p1))
nodes[1] <- T
i <- 1
j <- 3

# Follow i and j until first node has depleted its children. Assume that is the end instead of
# picking back up again at the end of the compelted chain.
while(i > 1 | p1[1] != -1) {
  if (p1[i] > 0) {
    nodes[j] <- TRUE      # This has to be a node because there are still child nodes left
    p1[i] <- p1[i] - 1    # Drop number of child nodes remaining by 1
    i <- j                # Move i to next node
    j <- j + 2            # Move j up two spots to get to next node
  } else {
    meta[j:(j + p1[i + 1] - 1)] <- TRUE   # The next x values are all meta
    p1[i] <- p1[i] - 1                    # Drop number of child nodes remaining by 1
    j <- j + p1[i + 1]                    # Account for new meta values in j
    i <- max(which(nodes[1:(i + 1)] & (p1 > -1)[1:(i + 1)]))     # Backtrack i to the next highest node
  }
}

# Print result
sum(p1[meta])