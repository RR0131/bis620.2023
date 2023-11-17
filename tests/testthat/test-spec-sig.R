test_that("multiplication works", {
  data(accel)
  expect_snapshot(accel |> head () |>spec_sig())

})
