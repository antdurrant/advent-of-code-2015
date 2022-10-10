# day 5

#strings
# nice MUST
#- have at least 3 vowels
#- have a repeated letter
#- NOT have ab cd pq or xy

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

l <- character()
for(i in seq_along(letters)){
  letter <- paste0(letters[i], ".", letters[i])
  if(length(l) == 0){
    l <- paste(letter)
  } else{
    l<- paste(l, letter, sep = "|")
  }
}




data.frame(input = read_lines("./data/day_5_data.txt")) %>%
  tidytext::unnest_character_shingles("shingles",input, 2, drop = FALSE) %>%
  group_by(input) %>%
  mutate(row = row_number()) %>%
  add_count(input, shingles) %>%
  filter(n >= 2,
         !(lag(row) == row | lead(row) == row))   %>% 
  count(input, shingles) %>%
  summarise(shingle_pass = max(n) >= 2) %>% 
  mutate(pair_pass = str_detect(input, l)) %>%
  group_by(input) %>%
  mutate(all_pass = shingle_pass + pair_pass) %>%
  ungroup() %>%
  count(all_pass)





