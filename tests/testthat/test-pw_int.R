pw <- pw_int(mod_dat, "mood", "ad", "post_int")

test_that("there are intervention columns", {
  expect_true("intervention" %in% names(pw))
})

test_that("data is extracted", {
  expect_true(sum(pw$n_dat_obs) > 0)
})
