test_that("cld_content() returns a named list of integer vectors", {
  skip_on_cran()
  expect_identical(class(cld_content()), "list")
  expect_identical(class(sample(cld_content(), 1)[[1]]), "integer")
})

test_that("cld_content() works with valid country codes", {
  skip_on_cran()
  expect_identical(length(cld_content("aut")[[1]]), 27L)
  expect_identical(length(cld_content("can")[[1]]), 44L)
  expect_identical(length(cld_content("cze")[[1]]), 9L)
  expect_identical(length(cld_content("esp")[[1]]), 14L)
  expect_identical(length(cld_content("fra")[[1]]), 16L)
  expect_identical(length(cld_content("deu")[[1]]), 20L)
  expect_identical(length(cld_content("irl")[[1]]), 33L)
  expect_identical(length(cld_content("sco")[[1]]), 6L)
  expect_identical(length(cld_content("gbr")[[1]]), 58L)
  expect_identical(length(cld_content("usa_house")[[1]]), 117L)
  expect_identical(length(cld_content("usa_senate")[[1]]), 117L)
})

test_that("cld_content() works with multiple country codes", {
  skip_on_cran()
  expect_silent(cld_content(c("aut", "deu")))
  expect_silent(cld_content(c("cze", "sco", "gbr")))
  expect_silent(cld_content(c("usa_house", "fra", "esp", "can")))
})

test_that("Error is returned when legislature argument is not a valid country code", {
  skip_on_cran()
  expect_error(cld_content(NA))
  expect_error(cld_content("bla"))
  expect_error(cld_content(c("deu", "bla")))
  expect_error(cld_content(2))
  expect_error(cld_content(TRUE))
})
