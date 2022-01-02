test_that("direction",
          {expect_equal(outcome_dir("qol"), "higher")
           expect_length(outcome_dir("qol"), 1)
            })

test_that("model type",
          {expect_equal(outcome_mod("qol"), "smd")
            expect_length(outcome_mod("qol"), 1)

            })
