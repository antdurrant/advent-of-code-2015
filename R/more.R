x <- c(20, 15,10,5,5)



y = 25

minus <- function(x, y){
    z <- as.numeric(sapply(x, \(x,y) ifelse(y == 0, y, y-x), y = y))
    
    n <- z[z>=0]
    y = n
    if(!all(n == 0)) Recall(x, n) else(length(n))
}


x <- c(50L, 44L, 11L, 49L, 42L, 46L, 18L, 32L, 26L, 40L, 21L, 7L, 18L, 43L, 10L, 47L, 36L, 24L, 22L, 40L)

# aoc2015_day17
day_17_1 <- function(x, y){
    res <- unlist(
        mapply(
            \(y) combn(x, y, FUN = sum),
            y = seq_along(x)
        )
    ) 
    sum(res == y)
}

day_17_1(x, 150)

day_17_2 <- function(x, y){
    
    z <-1
    while(!any(combn(x, z, FUN = sum) == y)){
        z <- z+1
    }
    
    sum(combn(x, z, sum) == y)
}
day_17_2(x,150)





# aoc2015_day14
speed = 14 
move = 10
rest = 127
total = 1000


get_nums <- function(x){
    m <- gregexec("\\d+", x)
    t(mapply(as.numeric,regmatches(x, m) ))
}

m <- get_nums(readLines(input))

day_14_1 <- function(speed, move, rest, total){
    sum(rep(c(rep(speed,move), rep(0, rest)), ceiling(total / (move+rest)))[1:total])
}

part_1 <-  function(m, total = 2503){
    max(mapply(FUN = day_14_1, m[,1], m[,2], m[,3], total))
}

part_1(m)

day_14_2 <- function(speed, move, rest, total){
    cumsum(rep(c(rep(speed,move), rep(0, rest)), ceiling(total / (move+rest)))[1:total])
}

part_2 <- function(m, total = 2503){
    r <- mapply(FUN = day_14_2, m[,1], m[,2], m[,3], total)
    max(colSums(r == apply(r, 1, max)))
}
part_2(m)


# aoc 2015 day 10
part_1 <- function(x, n = 40){
    
    m=1
    for(i in seq(n)){
        y <- rle(strsplit(x, "")[[1]])
        x <- paste0(y$lengths, y$values, collapse = "")
    }
    nchar(x)
}
part_1("1113222113")
part_1("1113222113", 50)


# day 11
x <- "hijklmmn"
contains_straight <- function(x){
    r <- rle(diff(sapply(strsplit(x, "")[[1]], \(x) which(letters == x))))
    any(r$lengths[r$values == 1] >= 2)
}

contains_doubles <- function(x) grepl("(.)\\1.*(.)\\2", x, perl = TRUE)
not_contains_iol <- function(x) !grepl("[iol]", x)

is_valid <- function(x) {
    x <- to_letters(x)
    contains_straight(x) & contains_doubles(x) & not_contains_iol(x)
}

to_nums <- function(x) sapply(strsplit(x, "")[[1]], \(x) which(letters == x))
to_letters <- function(x) paste(letters[x], collapse = "")
cycle <- function(x, pos = length(x)) {
    if(x[pos] < 26) {
        x[pos] <- x[pos]+1
        return(x)
    } else{
        x[pos] <- 1
        Recall(x, pos = (pos-1))
     }
}

add_one_pw <- function(x){
    new <- cycle(to_nums(x))
    while(!is_valid(new)){
        new <- cycle(new)
    }
    to_letters(new)
}
x = new
add_one_pw(x = "vzbxkghb")


# day 12
get_nums <- function(x){
    m <- gregexec("[-|\\d]+", x, perl = TRUE)
    sapply(regmatches(x, m), as.numeric)
}



# part 1
f <- file.choose()
read_file(f) |>
get_nums()  |>
    sum()


# part 2 
# ooer recursiveness
x <- jsonlite::read_json(f) 

y <- Filter(\(x) !any(x == "red"), x)

list_filter <- function(x){
    y <- Filter(\(x) !any(x == "red"), x)
    if(is.list(y)) Map(Filter(\(x) !any(x == "red")), y) 
}

z <- rapply(x[[1]], list_filter, how = "replace")

z <-rapply(x[[1]], \(y) Filter(\(x) !any(x == "red"), y), deflt = NULL, how = "list")

fff <- list(list(a = "red"), b = list(a = "red", b = 1), c= 2)

fff == "red"


