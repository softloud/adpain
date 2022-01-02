# will be useful to compare/use ideas betwee pw and nma


# set dat ----------------------------------------------------------------

get_pw_dat_output <- get_pw_dat(
  mod_dat,
  outcome = "pain_sub",
  type = "ad",
  timepoint = "post_int",
  g1 = "duloxetine",
  g2 = "placebo"
)



# test pw -----------------------------------------------------------------

test_that("pw", {
  expect_s3_class(get_pw_dat_output, "data.frame")
})

test_that("pw one outcome", {
  expect_true((get_pw_dat_output$outcome %>% unique() %>%
                 length()) == 1)
})

test_that("control is placebo", {
  expect_true((get_pw_dat_output$intervention_control %>%
                 unique()) == "placebo")

})

# test nma ----------------------------------------------------------------

get_nma_dat_output <- get_nma_dat(dat = mod_dat,
                                  outcome == "pain_sub",
                                  type == "ad",
                                  timepoint == "post_int")


test_that("nma", {
  expect_s3_class(get_nma_dat_output, "data.frame")
})


test_that("nma one outcome", {
  expect_true((get_nma_dat_output$outcome %>% unique() %>%
                 length()) == 1)
})

test_that("at least 2 obs per study", {
  one_arm <-
    get_nma_dat_output %>%
    group_by(outcome, timepoint) %>%
    count(study) %>%
    filter(n < 2)

  expect_true(nrow(one_arm) == 0)
})
