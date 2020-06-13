test_that("stooq get", {

  resp <- stooq_get("PKP")

  expect_s3_class(resp, "data.frame")
  expect_gt(nrow(resp), 10)


})
