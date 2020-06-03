library(testthat)
library(legislatoR)

test_check("legislatoR", filter = "cld_content")
test_check("legislatoR", filter = "get_1")
test_check("legislatoR", filter = "get_2")

