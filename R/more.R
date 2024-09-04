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


# day 18 ----

    
life <- function(m){
    
    nc <- ncol(m)
    nr <- nrow(m)    
    
    # vertical ---
    up <- rbind(m[2:nr,],0)
    down <- rbind(0, m[1:(nr-1),])
    
    # horizontal ----
    left <- cbind(m[,2:nc],0)
    right <- cbind(0, m[,1:(nc-1)])
    
    # diagonals ----
    upleft <- cbind(up[,2:nc], 0)
    downleft <- cbind(down[,2:nc],0)
    
    upright <- cbind(0, up[,1:(nc-1)])
    downright <- cbind(0, down[,1:(nc-1)])      

    neighbours <- up + down + left + right + upleft + downleft + upright + downright
    
    # rules
    
    n <- m
    n[m == 0 & neighbours == 3] <- 1
    n[m == 1 & !neighbours %in%  2:3] <- 0
    n
}

corners_on <- function(m){
    m[1,1] <- 1
    m[1,ncol(m)] <- 1
    m[nrow(m),1] <- 1
    m[nrow(m), ncol(m)] <- 1
    m
}


convert_matrix <- function(x) x == "#"


life_2 <- function(m){
    corners_on(life(m))
}

parse_input <- function(x){
    readr::read_lines(x) |>
        stringr::str_split("", simplify = TRUE) |>
        convert_matrix()
}


call_many <- function(fn, times, m){
    eval(parse(text = paste("m", paste(rep(fn, times), collapse = " |> "), sep = " |> ")))
}


part_1 <- function(x){
    m <- parse_input(x)
    sum(call_many("life()", 100, m))
}

part_2 <- function(x){
    m <- corners_on(parse_input(x))
    sum(call_many("life_2()", 100, m))
}

demo_data <- ".#.#.#
...##.
#....#
..#...
#.#..#
####.."

part_1(demo_data)
part_2(demo_data)

input <- "actual input"

part_1(input)
part_2(input)   


# aoc2015 day 15 ----
ex <- "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3" 


# demo
d <- matrix(c(-1,-2,6,3,8,2,3,-2,-1,3), ncol = 2)

df <- tibble(a = 0:100)
# demo
wts <- 
    df %>%
    mutate(b = map_dbl(a, ~(100-.x))) %>%
    apply(1, unname, simplify = FALSE)
# real
wts <- 
    df %>%
    mutate(b = map(a, ~0:(100-.x))) %>%
    unnest(b) %>%
    mutate(c = map2(a,b, ~0:(100-.x-.y))) %>%
    unnest(c) %>%
    mutate(d = pmap_dbl(list(a,b,c), ~100-..1-..2-..3)) %>%
    apply(1, identity, simplify = FALSE)



pd <- function(d, y){
    m <- rowSums(t(apply(d[1:4,], 1, \(x) x*matrix(y, ncol = 2))))
    prod(ifelse(m<0, 0,m))
}

wts

d <-
    "Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8" %>%
    str_extract_all("[-\\d]+") %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = 4)

d <- matrix(c(2,0,-2,0,3,0,5,-3,0,3,0,0,5,-1,8,0,-1,0,5,8), ncol = 4)

pd(d, wts[[45]])
max(mapply(pd, wts, MoreArgs = list(d= d)))

# pt2 - total cals must be 500
new_wts <- Filter(\(x) sum(d[5,] * x) == 500, wts) 
max(mapply(pd, new_wts, MoreArgs = list(d= d)))


