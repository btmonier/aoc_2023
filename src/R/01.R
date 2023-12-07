# /// AOC - DAY 01 //////////////////////////////////////////////////

## Data ----
inputPath <- "input/01.txt"

testInputP01 <- list(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
)

testInputP02 <- list(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
)



## Part 1 ----

### Solver
partOneSolver <- function(l) {
    solution <- l |>
        lapply(\(it) {
           x <- strsplit(it, "") |>
               unlist() |>
               as.numeric() |>
               suppressWarnings()

           x <- x[!is.na(x)] |> as.character()

           paste0(x[1], x[length(x)]) |>
               as.numeric()
        }) |>
        unlist() |>
        sum()

    return(solution)
}


### Test
testthat::expect_equal(
    testInputP01 |> partOneSolver(),
    142
)


### Actual
cat("=== PART ONE ===\n")
inputPath |>
    readLines() |>
    partOneSolver() |>
    print()



## Part 2 ----

### Solver (need to account for weird mergings)
txtNumMap <- c(
    "one"   = "o1e",
    "two"   = "t2o",
    "three" = "t3e",
    "four"  = "f4r",
    "five"  = "f5e",
    "six"   = "s6x",
    "seven" = "s7n",
    "eight" = "e8t",
    "nine"  = "n9e"
)

partTwoSolver <- function(l) {
    solution <- l |>
        lapply(\(it) {
            for (pattern in names(txtNumMap)) {
                it <- gsub(pattern, txtNumMap[[pattern]], it)
            }

            x <- strsplit(it, "") |>
                unlist() |>
                as.numeric() |>
                suppressWarnings()

            x <- x[!is.na(x)] |> as.character()
            x <- paste0(x[1], x[length(x)]) |> as.numeric()

            return(x)
        }) |>
        unlist() |>
        sum()

    return(solution)
}


### Test
testthat::expect_equal(
    testInputP02 |> partTwoSolver(),
    281
)


### Actual
cat("=== PART TWO ===\n")
inputPath |>
    readLines() |>
    partTwoSolver() |>
    print()


