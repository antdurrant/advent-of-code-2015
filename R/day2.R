
wrap <- function(dims){
  x <- dims[1]*dims[2]
  y <- dims[2]*dims[3]
  z <- dims[1]*dims[3]
  (2*x) + (2*y) + (2*z) + min(x,y,z)
}


ribbon <- function(dims){
  x <- sort(dims)[1:2]
  sum(prod(dims), (2*x))
}


input <- Map(as.numeric, strsplit(readLines("./data/day_2_data.txt"), "x"))

sum(unlist(Map(wrap, input)))

sum(unlist(Map(ribbon, input)))

