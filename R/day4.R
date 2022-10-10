tools::md5sum("./data/day_4_data.txt")
# part 1
# find the first md5 hash that starts with 5 zeroes
# where the hashed thing is your input followed by a number

# part 2 starts with 6 zeroes

tictoc::tic()
res <- 0
x <- 0
while( res == 0 ){

  if(str_detect(cli::hash_md5(paste0("iwrupvqb", x)), "^000000")){
    res <- x
} else{
  x <- x+1
}
  res
}
x
cli::hash_md5(paste0("iwrupvqb", x))
tictoc::toc()


