test_that("hash is working as in bash", {
  expect_equal(makeSecret("test"), "098f6bcd4621d373cade4e832627b4f6")
})
