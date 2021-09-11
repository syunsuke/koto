test_that("road_cond_decode関数のテスト", {
  expect_equal(road_cond_decode(c("1","2","5")), c("側道","三方路","背面道"))
})
