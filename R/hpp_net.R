hpp_net <- function(dat, type) {

  n_placebo <-
    dat %>%
    filter(intervention == "placebo") %>%
    nrow()

  if (n_placebo == 0) stop("no placebo")

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