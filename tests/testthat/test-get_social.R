test_that("Social table is returned appropriately for each legislature", {
  skip_on_cran()
  expect_identical(class(get_social("aut")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("can")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("cze")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("esp")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("fra")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("deu")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("irl")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("sco")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("gbr")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("usa_house")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_social("usa_senate")), "data.frame")
  Sys.sleep(100)

  expect_true(all(dim(get_social("aut")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("can")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("cze")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("esp")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("fra")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("deu")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("irl")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("sco")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("gbr")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("usa_house")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_social("usa_senate")) > 0))
  Sys.sleep(100)

  expect_identical(colnames(get_social("aut"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("can"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("cze"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("esp"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("fra"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("deu"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("irl"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("sco"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("gbr"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("usa_house"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_social("usa_senate"))[1], c("wikidataid"))
  Sys.sleep(100)
})
