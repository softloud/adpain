hpp_net <- function(dat, type) {

  n_placebo <-
    dat %>%
    filter(intervention == "placebo") %>%
    nrow()

  ref <- 
    ifelse(
      nrow(n_placebo) > 0,
      "placebo",
      NULL
    )

  if (type == "smd") {
    set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      trt_ref = "placebo",
      y = mean,
      se = se,
      sample_size = n
    )
  } else if (type == "lor") {
    set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      trt_ref = "placebo",
      r = r,
      n = n,
      sample_size = n
    )  }
}