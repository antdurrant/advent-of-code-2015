# test the actions on matrices
#https://adventofcode.com/2015/day/6

x <- matrix(0, ncol = 1000, nrow = 1000)
x[1:1000, 1:1000] <- x[1:1000, 1:1000] +1
x[1:1000, 1] <- x[1:1000, 1] +1
x[500:501, 500:501] <- x[500:501, 500:501] +1

sum(x %% 2 == 1)


# actions ----

turn_on <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- TRUE
  mat
}

turn_off <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- FALSE
  mat
}

toggle <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- !mat[x$first:x$last, y$first:y$last]
  mat
}


# parse the numbers ----

# R is 1-indexed
# the instructions explicitly say they count from 0

parse_x <- function(input){
  first <- as.numeric(str_extract(input, "\\d+"))+1
  last <- as.numeric(str_extract(input, "(?<=through )\\d+"))+1
  list(first = first, last = last)
}

parse_y <- function(input){
  first <- as.numeric(str_extract(input, "(?<=\\d{1,3},)\\d+"))+1
  last <- as.numeric(str_extract(input, "(?<=through \\d{1,3},)\\d+"))+1
  list(first = first, last = last)
}

# put it together ----

parse_modification <- function(mat, input){
  type <- str_extract(input, "off|on|toggle")
  switch(type,
         off = turn_off(mat, parse_x(input), parse_y(input)),
         on = turn_on(mat, parse_x(input), parse_y(input)),
         toggle = toggle(mat, parse_x(input), parse_y(input))
  )
}

# test behaviour ----

m <- matrix(data = 0, nrow = 3, ncol = 3)
n <- m
test_case <- c(
  "toggle 0,0 through 1,1",
  "turn off 0,0 through 0,1",
  "turn on 2,1 through 2,2"
)

# practicing reduce for this kind of thing
# purrr
test_res <-reduce(test_case, parse_modification, .init = m)
# base
test_res2 <- Reduce(parse_modification, test_case, m)

n[1:2, 1:2] <- !n[1:2, 1:2]
n[1:1, 1:2] <- 0
n[3:3, 2:3] <- 1
n

identical(n, test_res)
identical(test_res, test_res2)

# part 1 in practice ----

input <- read_lines("./data/day_6_data.txt")
mat <- matrix(0, ncol = 1000, nrow = 1000)

res <- reduce(input, parse_modification, .init = mat)  

sum(res %% 2 == 1)
#> [1] 400410

# part 2 ----

# now toggle = +2
# now turn on = +1
# now turn off = -1

# what is the total brightness?


# check it does what i think it does
m
m[1,1] <- m[1,1] + 2
m

# redefine the three helpers

turn_on <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- mat[x$first:x$last, y$first:y$last] + 1
  mat
}

turn_off <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- mat[x$first:x$last, y$first:y$last]-1
  # lowest permissible level is 0
  mat[mat < 0] <- 0
  mat
}

toggle <- function(mat, x, y){
  mat[x$first:x$last, y$first:y$last] <- mat[x$first:x$last, y$first:y$last] +2
  mat
}

# check it _is_ doing what i expect
turn_off(m, list(first = 2, last = 2), list(first = 2, last = 2))


res <- reduce(input, parse_modification, .init = matrix(data = 0, nrow = 1000, ncol = 1000))  
sum(res)
