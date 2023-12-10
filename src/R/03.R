# /// AOC - DAY 03 //////////////////////////////////////////////////

## Data ----
inputPath <- "input/03.txt"

testInputP01 <- list(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
)

testInputP02 <- testInputP01



## Helper functions ----

### Convert input to matrix
inputToMatrix <- function(l) {
    m <- l |>
        lapply(strsplit, split = "") |>
        unlist() |>
        matrix(
            nrow  = length(l),
            ncol  = l[[1]] |> nchar(),
            byrow = TRUE
        )

    # Edge cases? mwahahaha
    m <- cbind(".", m, ".")
    m <- rbind(".", m, ".")

    return(m)
}



## Part 1 ----

### Solver
partOneSolver <- function(l) {
    m <- l |> inputToMatrix()

    nRow <- nrow(m)
    nCol <- ncol(m)

    validNums <- c()
    for (i in m |> nrow() |> seq_len()) {
        nums    <- list()
        nId     <- 0
        currNum <- character(0)
        nMIndex <- numeric(0)
        for (j in m[i, ] |> seq_along()) {
            if (grepl("[0-9]", m[i, j])) {
                currNum <- paste0(currNum, m[i, j])
                nMIndex <- c(nMIndex, j)
            } else {
                if (length(currNum) != 0) {
                    nId         <- nId + 1
                    nums[[nId]] <- list(
                        "number" = currNum |> as.numeric(), # valid num in m
                        "nIndex" = nMIndex # indices num resides in m
                    )
                    currNum     <- character(0)
                    nMIndex     <- numeric(0)
                }
            }
        }

        if (i %in% seq(2, nrow(m) - 1)) {
            if (length(nums) != 0) {
                nums |>
                    lapply(\(it) {
                        left  <- it$nIndex |> min() - 1
                        right <- it$nIndex |> max() + 1
                        up    <- c(left, it$nIndex, right)
                        down  <- c(left, it$nIndex, right)

                        hits <- !grepl(
                            "\\.",
                            x = c(
                                m[i, c(left, right)],
                                m[i + 1, down],
                                m[i - 1, up]
                            )
                        )

                        if (any(hits)) {
                            validNums <<- c(validNums, it$number)
                        }
                    })
            }

        }
    }

    return(validNums |> sum())
}


### Test
testthat::expect_equal(
    testInputP01 |> partOneSolver(),
    4361
)


### Actual
cat("=== PART ONE ===\n")
inputPath |>
    readLines() |>
    partOneSolver() |>
    print()



## Part 2 ----

### Solver
partTwoSolver <- function(l) {
    m <- l |> inputToMatrix()

    nRow <- nrow(m)
    nCol <- ncol(m)

    metaNum  <- list()
    metaGear <- list()
    nId      <- 0
    gId      <- 0
    for (i in m |> nrow() |> seq_len()) {
        nums    <- list()
        gears   <- list()
        currNum <- character(0)
        nMIndex <- numeric(0)
        for (j in m[i, ] |> seq_along()) {
            if (grepl("[0-9]", m[i, j])) {
                currNum <- paste0(currNum, m[i, j])
                nMIndex <- c(nMIndex, j)
            } else {
                if (length(currNum) != 0) {
                    nId         <- nId + 1
                    metaNum[[nId]] <- list(
                        "number" = currNum |> as.numeric(), # valid num in m
                        "nIndex" = nMIndex, # indices num resides in m
                        "row"    = i
                    )
                    currNum     <- character(0)
                    nMIndex     <- numeric(0)
                }
            }

            if (m[i, j] == "*") {
                gId <- gId + 1
                metaGear[[gId]] <- list(
                    "row" = i,
                    "col" = j
                )
            }

        }

    }

    validNums <- numeric(0)
    for (gear in metaGear) {
        validRows <- c(gear$row - 1, gear$row, gear$row + 1)
        validCols <- c(gear$col - 1, gear$col, gear$col + 1)
        subNums <- metaNum |>
            Filter(
                \(it) {
                    it$row %in% validRows &
                    any(it$nIndex %in% validCols)
                },
                x = _
            )
        if (length(subNums) == 2) {
            validNums <- c(validNums, subNums[[1]]$number * subNums[[2]]$number)
        }
    }

    return(validNums |> sum())
}


### Test
testthat::expect_equal(
   testInputP02 |> partTwoSolver(),
   467835
)


### Actual
cat("=== PART TWO ===\n")
inputPath |>
    readLines() |>
    partTwoSolver() |>
    print()


