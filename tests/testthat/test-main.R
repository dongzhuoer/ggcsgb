context("Testing main")
setwd(here::here(''))  # workspace is reset per file

testthat::test_that("Testing chrono_strati_bar()", {
    testthat::expect_true(TRUE)
	ggtree::ggtree(ape::rcoal(100)) + chrono_strati_arg(333, 1) + chrono_strati_bar() 
})

testthat::test_that("Testing chrono_strati_axis()", {
    testthat::expect_true(TRUE)
	ggtree::ggtree(ape::rcoal(100)) + chrono_strati_arg(333, 1) + chrono_strati_axis() 
})

testthat::test_that("Testing chrono_strati_label()", {
    testthat::expect_true(TRUE)
	ggtree::ggtree(ape::rcoal(100)) + chrono_strati_arg(333, 1) + chrono_strati_label() 
})
