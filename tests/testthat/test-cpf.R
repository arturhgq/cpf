test_that("cpf functions", {
  expect_equal(
    add_digits("22344533"),
    "00022344533"
  )
  expect_equal(
    fix_cpf("00022344533"),
    "000.223.445-33"
  )
})

