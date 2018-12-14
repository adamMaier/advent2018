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
nodes <- meta <- rep(F, length(p1))
nodes[1] <- T
parent_node <- rep(NA, length(p1))
i <- 1
j <- 3

# Follow i and j until first node has depleted its children. Assume that is the end instead of
# picking back up again at the end of the compelted chain.
while(i > 1 | p1[1] != -1) {
  if (p1[i] > 0) {
    nodes[j] <- TRUE      # This has to be a node because there are still child nodes left
    parent_node[[j]] <- i # Provides position of parent node
    p1[i] <- p1[i] - 1    # Drop number of child nodes remaining by 1
    i <- j                # Move i to next node
    j <- j + 2            # Move j up two spots to get to next node
  } else {
    meta[j:(j + p1[i + 1] - 1)] <- TRUE      # The next x values are all meta
    parent_node[j:(j + p1[i + 1] - 1)] <- i  # These are the parent nodes for these meta values
    p1[i] <- p1[i] - 1                       # Drop number of child nodes remaining by 1
    j <- j + p1[i + 1]                       # Account for new meta values in j
    i <- suppressWarnings(max(which(nodes[1:(i + 1)] & (p1 > -1)[1:(i + 1)])))  # Backtrack i to the next highest node
  }
}

# Print result
sum(p1[meta])



# PART 2 -------------------------------------------------------------------------------------------

p2 <- as.numeric(data)

# Function to find meta values of given node
meta_finder <- function(x) {
  p2[parent_node == x & meta]
}

# Function to find position of child values of given node
child_finder <- function(x) {
  which(nodes & parent_node == x & !meta)
}

# Create loop that stores sum of values when it hits a child vector with no more children and
# replaces vector of current children
init <- child_finder(1)[meta_finder(1)]
init_remain <- 0
out_sum <- 0

while(length(init_remain) > 0) {
  
  # Reset vector
  init_remain <- c()
  
  # Remove instances where child doesn't exist
  init <- init[!is.na(init)]
  if(length(init) == 0) init_remain <- c()

  # Loop through child nodes and find those that have no more children, to add their meta to the 
  # final count
  for(i in init) {
    if(p2[i] == 0) {
      out_sum <- out_sum + sum(meta_finder(i))
    } else {
      init_remain <- c(init_remain, i)
    }
  }
  
  # Find next set of child nodes
  init <- c()
  for(j in init_remain) {
    init <- c(init, child_finder(j)[meta_finder(j)])
  }
}

# Print result
out_sum