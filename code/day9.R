# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, readr, stringr, lubridate, purrr)



# PART 1 -------------------------------------------------------------------------------------------

# Provide and calcualte initial values
marble_chain <- 0
marble_pos <- 1
new_marble <- 1
player <- 1
total_players <- 438
player_points <- rep(0, total_players)
highest_marble <- 71626


# Loop through different options until final marble is playes
while(new_marble <= highest_marble) {

  player_index <- total_players - (player %% total_players)

  if (new_marble %% 23 != 0) {
  
    if (marble_pos + 2 <= length(marble_chain)) {
      marble_chain <- c(
        marble_chain[1:(marble_pos + 1)], 
        new_marble, 
        marble_chain[(marble_pos + 2):length(marble_chain)]
      )
      marble_pos <- marble_pos + 2
    } else if (marble_pos + 1 == length(marble_chain)) {
      marble_chain <- c(marble_chain, new_marble)
      marble_pos <- length(marble_chain)
    } else if (marble_pos == length(marble_chain)) {
      marble_chain <- c(marble_chain[1], new_marble, marble_chain[-1])
      marble_pos <- 2
    }

  }
  
  if (new_marble %% 23 == 0) {
    
    if ((marble_pos - 7) > 0) {
      new_points <- new_marble + marble_chain[marble_pos - 7]
      marble_chain <- marble_chain[-(marble_pos - 7)]
      marble_pos <- marble_pos - 7
    } else {
      new_points <- new_marble + marble_chain[length(marble_chain) - (7 - marble_pos)]
      marble_chain <- marble_chain[-(length(marble_chain) - (7 - marble_pos))]
      marble_pos <- ifelse(marble_pos - 7 == 0, 1, length(marble_chain) - (6 - marble_pos))
    }
    
    player_points[player_index] <- player_points[player_index] + new_points
  
  }
  
  player <- player + 1
  new_marble = new_marble + 1
  
}

# Return results
max(player_points)