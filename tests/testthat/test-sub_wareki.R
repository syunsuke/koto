test_that("make_wareki関数のテスト", {

  expect_equal(make_wareki(1968),"S43")
  expect_equal(make_wareki(1989),"S64/H01")
  expect_equal(make_wareki(1990),"H02")
  expect_equal(make_wareki(2019),"H31/R01")
  expect_equal(make_wareki(2022),"R04")

})
