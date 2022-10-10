library(tidyverse)

# tidy
tictoc::tic()
x <- read_file("./data/day_1_data.txt") %>%
  str_split("") %>%
  unlist() %>%
  str_replace_all("\\)", "-1") %>%
  str_replace("\\(", "+1") %>% 
  parse_number() 

sum(x)

which(cumsum(x)==-1)[1]
tictoc::toc()


# base
tictoc::tic()
change <- function(x){
  switch(x,
    `(` = 1,
     `)` = -1
  )
}

input <- unlist(strsplit(readLines("./data/day_1_data.txt"), ""))
x <- unlist(Map(change, input))

sum(x)
which(cumsum(x) == -1)[1]

tictoc::toc()
