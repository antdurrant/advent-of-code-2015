
library(bitops)
123 -> x
456 -> y
bitwAnd(x, y) -> d
bitwOr(x, y) -> e
bitwShiftL(x, 2) -> f
bitwShiftR(y, 2) -> g
# bitwNot does not behave as the demonstrated behaviour
# bitwidth needs to be 16, i guess
bitops::bitFlip(x, 16) -> h
bitops::bitFlip(y, 16) -> i


123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

m <- "123 -> x"
str2lang("124 -> l")
j <- parse(text = "123 -> l")
j
eval(parse(text = c("128 -> l", "127 -> k", "'aaa' -> p")))

library(tidyverse)

input <- readLines("./data/day_7_data.txt")

content <- strsplit(input, split = " -> ") %>%
  map(~setNames(.x, c("content", "object")))

parse_text <- function(text){
  case_when(
    str_detect(text, " AND ") ~ 
      paste0("bitwAnd(", text, ")") %>% 
      str_replace(" AND ", ", "),
    str_detect(text, " OR ") ~ 
      paste0("bitwOr(", text, ")") %>% 
      str_replace(" OR ", ", "),
    str_detect(text, " LSHIFT ") ~
      paste0("bitwShiftL(", text, ")") %>%
      str_replace(" LSHIFT ", ", "),
    str_detect(text, " RSHIFT ") ~
      paste0("bitwShiftR(", text, ")") %>%
      str_replace(" RSHIFT ", ", "),
    str_detect(text, "NOT ") ~ 
      paste0("bitops::bitFlip(", text, ", 16)") %>% 
      str_remove("NOT")
  ) 
}


sort(input)
content

instructions <-
  
  map_chr(
    content, 
    ~paste(parse_text(.x["content"]), .x["object"], sep = " -> ")
    ) %>% 
  str_replace_all("\\bin\\b", "`in`") %>%
  str_replace_all("\\bif\\b", "`if`") %>%
eval(parse(text = instructions))



for(i in seq_along(instructions)){
  eval(parse(text = instructions[i]))
}

bitwAnd('fo', 'fz')
