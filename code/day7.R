# Load packages and clear workspace
rm(list = ls())
pacman::p_load(dplyr, magrittr, tidyr, readr, stringr, lubridate, purrr)

# Read in data
data <- 
  read_delim(
    "~/repos/advent2018/data/day7.txt", 
    delim = " ",
    col_types = "-c-----c--",
    col_names = c("pre", "post")
  )



# PART 1 -------------------------------------------------------------------------------------------

# Create data.frame with all letters
letters_data <- data.frame(post = LETTERS, stringsAsFactors = F)

# For each letter in alphabet find all letters that have to happen before it. Put them in a list 
# element
p1 <- data %>%
  group_by(post) %>%
  summarize(pre_string = paste0(pre, collapse = "")) %>%
  ungroup() %>%
  full_join(letters_data) %>%
  mutate(pre_string = ifelse(is.na(pre_string), "", pre_string)) %>%
  split(.$post) %>%
  map(~pull(., pre_string))

# Performing 26 iterations of finding list element with minimum length (and alpha if tied), 
# storing it, then removing its letter from remaining list.
chain <- c()
for(i in 1:26){
  
  # Calculate minimum number of characters in pre-requirement, then the first value since they're
  # already alpha sorted
  a <- map_dbl(p1, nchar)
  b <- a[which(a == min(a))]
  c <- names(b[1])
  
  # Remove chosen letter from every list element, and element itself
  p1 <- map(p1, ~str_remove_all(., c))
  p1[[c]] <- NULL
  
  # Store in chain
  chain[i] <- c
}

# Print result
paste0(chain, collapse = "")



# PART 2 -------------------------------------------------------------------------------------------

# Create data.frame with all letters
letters_data <- data.frame(post = LETTERS, stringsAsFactors = F)

# For each letter in alphabet find all letters that have to happen before it. Put them in a list 
# element
p2 <- data %>%
  group_by(post) %>%
  summarize(pre_string = paste0(pre, collapse = "")) %>%
  ungroup() %>%
  full_join(letters_data) %>%
  mutate(pre_string = ifelse(is.na(pre_string), "", pre_string)) %>%
  split(.$post) %>%
  map(~pull(., pre_string))

# Function to drop letters if completed
drop_letters <- function(data_list, drop_vec) {
  if(length(drop_vec) > 0) {
    out <- map(data_list, ~str_remove_all(., drop_vec))
    out[[drop_vec]] <- NULL
  } else {
    out <- data_list
  }
  out
}

# Function that finds all eligible letters
elig_letters <- function(x) {
  a <- map_dbl(x, nchar)
  names(a[which(a == 0)])
}

# Function that finds eligible elves and puts them in a list
elig_elves <- function(index, input_list) {
  log_vec <- map(input_list, index)
  yes <- input_list[is.na(log_vec)]
  no <- input_list[!is.na(log_vec)]
  list(yes, no)
}
  
# Function that returns a vector of commited time for an elf
commit_elf <- function(elf_vec, letter) {
  out <- elf_vec
  out[length(out):(length(out) + 60 + which(LETTERS == letter) - 1)] <- letter
  out[length(out) + 1] <- NA
  out
}

# Set-up trackers
elf_1 <- elf_2 <- elf_3 <- elf_4 <- elf_5 <- NA_character_
elf_list <- list(elf_1, elf_2, elf_3, elf_4, elf_5)
letter_ends <- rep(0, 26)
names(letter_ends) <- LETTERS
i <- 0
test <- FALSE

# Run loop until all NAs at the end.
while (!test) {
  
  # Start counter
  i <- 1 + i
  
  # Drop any letters if now completed
  to_drop <- names(letter_ends[which(letter_ends == i)])
  p2 <- drop_letters(p2, to_drop)

  # Find if any letters are eligible
  open_letters <- elig_letters(p2)
  
  # Remove eligible letters if they already have been claimed
  open_letters <- open_letters[open_letters %in% names(letter_ends[letter_ends == 0])]
  
  # Find if elves that are available and not
  open_elves <- elig_elves(i, elf_list)[[1]]
  used_elves <- elig_elves(i, elf_list)[[2]]

  # If letters equals or exceeds open elves, assign all elves. Otherwise assign non-assigned elves
  # an NA for 'available'
  if (length(open_letters) >= length(open_elves) & length(open_elves) != 0) {
    for(j in 1:length(open_elves)) {
      open_elves[[j]] <- commit_elf(open_elves[[j]], open_letters[j])
      letter_ends[which(names(letter_ends) == open_letters[j])] <- i + 60 + which(LETTERS == open_letters[j])
    }
  } 
  if (length(open_letters) < length(open_elves) & length(open_elves) != 0) {
    if(length(open_letters) > 0) {
      for(k in 1:length(open_letters)) {
        open_elves[[k]] <- commit_elf(open_elves[[k]], open_letters[k])
        letter_ends[which(names(letter_ends) == open_letters[k])] <- i + 60 + which(LETTERS == open_letters[k])
      }
    }
    for(l in (1 + length(open_letters)):length(open_elves)) {
      open_elves[[l]] <- c(open_elves[[l]], NA_character_)
    }
  }
  
  # Recombine elf lists
  elf_list <- c(open_elves, used_elves)
  
  # Run test to stop loop
  test <- sum(is.na(map(elf_list, i))) == 5
}

# Print counter (subtracting 1 for 1 for end)
i - 1