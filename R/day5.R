# day 5

#strings
# nice MUST
#- have at least 3 vowels
#- have a repeated letter
#- NOT have ab cd pq or xy

library(tidyverse)

# prep for MUST #2
l <- character()
for(i in seq_along(letters)){
  letter <- paste0(rep(letters[i], 2), collapse = "")
  if(length(l) == 0){
    l <- paste(letter)
  } else{
  l<- paste(l, letter, sep = "|")
  }
}


test_string <- function(string){
  if(str_detect(string, "xy|ab|cd|pq")){
    return("naughty")
  }
  
  if(str_count(string, "a|e|i|o|u") >= 3 &
    str_detect(string,  l)){
    "nice"
  } else {
    "naughty"
  }
  
}


x <- map(read_lines("./data/day_5_data.txt") , test_string)

length(x[x == "nice"])



# prep for MUST #2
input <- readLines("./data/day_5_data.txt")

library(stringr)

test_case <- c("qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy")

# capturing groups! 
# https://r4ds.hadley.nz/regexps.html#grouping-and-capturing
# \\1 == same thing as first parens

# returns true/false on finding at least one match
repeated_pair <- function(x) {
  str_count(x, "(..).*\\1") > 0
}


# returns true/false on finding at least one match
sandwich <- function(x){
  str_count(x, "(.).\\1") > 0
}

# vector of # tests passed
# length of both passing
how_many_nice <- function(x){
  res <- repeated_pair(x) + sandwich(x)
  length(res[res == 2])
}


how_many_nice(test_case)

how_many_nice(input)
