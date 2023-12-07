# /// AOC - DAY 02 //////////////////////////////////////////////////

## Data ----
inputPath <- "input/02.txt"

testInputP01 <- list(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)

testInputP02 <- list(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)



## Part 1 ----

### Solver
maxCubes <- list(
    "red"   = 12,
    "green" = 13,
    "blue"  = 14
)

partOneSolver <- function(l) {
    validGames <- c()
    l |>
        lapply(\(it) {
            currGame <- it |> gsub("Game |:.*$", "", x = _) |> as.numeric()

            gameRes <- it |>
                gsub("Game .*: ", "", x = _) |>
                strsplit("; ") |>
                unlist() |>
                strsplit(", ")

            nNotValid <- 0

            gameRes |> lapply(\(ev) {
                for (mc in maxCubes |> names()) {
                    cubeValue <- ev[grepl(mc, ev)] |>
                        gsub(" .*$", "", x = _) |>
                        as.numeric() |>
                        sum()

                    if (cubeValue > maxCubes[[mc]]) {
                        nNotValid <<- nNotValid + 1
                    }
                }
            })

            if (nNotValid == 0) {
                validGames <<- c(validGames, currGame)
            }
        })

    return(validGames |> sum())
}


### Test
testthat::expect_equal(
    testInputP01 |> partOneSolver(),
    8
)


### Actual
cat("=== PART ONE ===\n")
inputPath |>
    readLines() |>
    partOneSolver() |>
    print()



## Part 2 ----

### Solver
maxCubes <- list(
    "red"   = 12,
    "green" = 13,
    "blue"  = 14
)

partTwoSolver <- function(l) {
    m <- l |>
        lapply(\(it) {
            currGame <- it |> gsub("Game |:.*$", "", x = _) |> as.numeric()

            gameRes <- it |>
                gsub("Game .*: ", "", x = _) |>
                strsplit("; ") |>
                unlist() |>
                strsplit(", ")

            maxRGBs <- list(
                "red"   = 0,
                "green" = 0,
                "blue"  = 0
            )

            gameRes |> lapply(\(ev) {
                for (mc in maxCubes |> names()) {
                    cubeValue <- ev[grepl(mc, ev)] |>
                        gsub(" .*$", "", x = _) |>
                        as.numeric() |>
                        sum()

                    if (cubeValue > maxRGBs[[mc]]) {
                        maxRGBs[[mc]] <<- cubeValue
                    }
                }
            })

            return(maxRGBs |> unlist() |> prod())

        })

    return(m |> unlist() |>  sum())
}


### Test
testthat::expect_equal(
    testInputP02 |> partTwoSolver(),
    2286
)


### Actual
cat("=== PART TWO ===\n")
inputPath |>
    readLines() |>
    partTwoSolver() |>
    print()


