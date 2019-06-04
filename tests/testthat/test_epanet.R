context("epanetgis")
library(epanetgis)
test_that("Test EPANET installation", {
  expect_false(en_open())
})

