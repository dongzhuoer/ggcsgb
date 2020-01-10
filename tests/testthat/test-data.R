testthat::context('Testing data')
setwd(here::here(''))  # workspace is reset per file

testthat::test_that("Phanerozoic", {
    testthat::expect_true(tibble::is_tibble(ggcsgb::Phanerozoic))
})
