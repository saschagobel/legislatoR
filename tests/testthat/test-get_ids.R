test_that("IDs table is returned appropriately for each legislature", {
  skip_on_cran()
  expect_identical(class(get_ids("aut")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("can")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("cze")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("esp")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("fra")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("deu")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("irl")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("sco")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("gbr")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("usa_house")), "data.frame")
  Sys.sleep(1)
  expect_identical(class(get_ids("usa_senate")), "data.frame")
  Sys.sleep(100)

  expect_true(all(dim(get_ids("aut")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("can")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("cze")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("esp")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("fra")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("deu")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("irl")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("sco")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("gbr")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("usa_house")) > 0))
  Sys.sleep(1)
  expect_true(all(dim(get_ids("usa_senate")) > 0))
  Sys.sleep(100)

  expect_identical(colnames(get_ids("aut"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("can"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("cze"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("esp"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("fra"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("deu"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("irl"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("sco"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("gbr"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("usa_house"))[1], c("wikidataid"))
  Sys.sleep(1)
  expect_identical(colnames(get_ids("usa_senate"))[1], c("wikidataid"))
  Sys.sleep(100)
})
