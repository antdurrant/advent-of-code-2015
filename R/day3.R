
library(tidyverse)

dat <- read_file("./data/day_3_data.txt") %>% 
  strsplit("") %>%
  unlist()


index <- function(x){
  switch(x,
         `>` = c(x = 1, y = 0),
         `<` = c(x = -1, y = 0),
         `^` = c(x = 0, y = 1),
         `v` = c(x = 0, y = -1)
  )
}



# start at 0,0
bind_rows(
  data.frame(x = 0, y = 0),
  data.frame(input = dat) %>%
    mutate(data = map(input, index)) %>%
    unnest_wider(data) 
) %>%
  mutate(csx = cumsum(x),
         csy = cumsum(y)) %>%
  count(csx, csy) %>%
  nrow()

# start at 0,0
bind_rows(
  data.frame(x = 0, y = 0), 
  data.frame(input = dat) %>%
    mutate(data = map(input, index)) %>%
    unnest_wider(data) 
) %>%
  group_by(row_number() %% 2) %>%
  mutate(csx = cumsum(x),
         csy = cumsum(y)) %>%
  ungroup() %>%
  count(csx, csy) %>%
  nrow()




nrow(base)

x <- Map(index, unlist(strsplit(">^", "")))
transpose(x)
y <- purrr::map( unlist(strsplit(">^", "")), index) %>% transpose() 

sum(y$x)
map(y, ~cumsum(unlist(.x))) %>% transpose()



x <- ">v^<"
y <- purrr::transpose(purrr::map(strsplit(x, "")[[1]], index ))

x_dim <- cumsum(y$x)
y_dim <- cumsum(y$y)

x_min <- min(x_dim)
x_max <- max(x_dim)

y_min <- min(y_dim)
y_max <- max(y_dim)
