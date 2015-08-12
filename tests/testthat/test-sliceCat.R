
test_that("sliceCat() throws errors/warnings when it should", {

  mydef <- list(
    a = list( fdp=c(0,   4), bdp=c(0,   6) ),
    b = list( fdp=c(0,   4), bdp=c(6, Inf) ),
    c = list( fdp=c(4, Inf) )
  )
  fishingD <- 1:7
  bottomD <- c(2, 10, 4, 12, 6, 14, 8)
  latitude <- 1:7

  ans <- c("a", "b", "a", "c", "c", "c", "c")

  # nulls: 0, 1, 2, 3
  expect_equal(sliceCat(mydef, fdp=fishingD, bdp=bottomD, lat=latitude), ans)
  expect_equal(sliceCat(mydef, fdp=fishingD, bdp=bottomD), ans)
  expect_error(sliceCat(mydef, fdp=fishingD))
  expect_error(sliceCat(mydef))

  # slices: overlapped or gapped
  overdef1 <- list(
    a = list( fdp=c(0,   4), bdp=c(0,   6) ),
    b = list( fdp=c(0,   4), bdp=c(6, Inf) ),
    c = list( fdp=c(2, Inf) )
  )
  overdef2 <- list(
    a = list( fdp=c(0,   4), bdp=c(0,   6) ),
    b = list( fdp=c(0,   4), bdp=c(2, Inf) ),
    c = list( fdp=c(4, Inf) )
  )
  gapdef1 <- list(
    a = list( fdp=c(0,   3), bdp=c(0,   6) ),
    b = list( fdp=c(0,   3), bdp=c(6, Inf) ),
    c = list( fdp=c(5, Inf) )
  )
  gapdef2 <- list(
    a = list( fdp=c(0,   4), bdp=c(0,   3) ),
    b = list( fdp=c(0,   4), bdp=c(6, Inf) ),
    c = list( fdp=c(4, Inf) )
  )
  gapdef3 <- list(
    a = list( fdp=c(0,   8) ),
    b = list( fdp=c(9, Inf) )
  )
  expect_warning(sliceCat(overdef1, fdp=fishingD, bdp=bottomD))
  expect_warning(sliceCat(overdef2, fdp=fishingD, bdp=bottomD))
  expect_warning(sliceCat(gapdef1, fdp=fishingD, bdp=bottomD))
  expect_warning(sliceCat(gapdef2, fdp=fishingD, bdp=bottomD))
  expect_warning(sliceCat(gapdef3, fdp=fishingD, bdp=bottomD))

})
