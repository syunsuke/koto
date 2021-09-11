test_that("decode_bitdata関数のテスト", {

  expect_equal(decode_bitdata_single("111",c("a", "b", "c")),
               c("a", "b", "c"))

  expect_equal(
    readable_string_for_bitdata(c("111", "010", "000"),c("a", "b", "c")),
    c("a・b・c", "b", ""))
})
