#' Pairwise set
#'
#' First group is treatment group, second control.
#'
#' @param dat Dataframe
#'
#' @export

hpp_rma <- function(dat) {
  m_type <- dat %>% pull(model_type) %>% unique()
  msg_mine(glue("m_type {m_type}"))

  hpp_measure <-
    if (m_type == "smd") "SMD" else if (m_type == "lor") "OR"

  escalc_dat <-
  if (m_type == "smd") {
    metafor::escalc(
      measure = hpp_measure,
      m1i = mean,
      m2i = mean_control,
      sd1i = sd,
      sd2i = sd_control,
      n1i = n,
      n2i = n_control,
      slab = study,
      data = dat
    )
  } else if (m_type == "lor") {
    metafor::escalc(
      measure = hpp_measure,
      ai = r_control,
      n1i = n_control,
      ci = r,
      n2i = n,
      slab = study,
      data = dat
    )
  }

  msg_mine(glue("escalc_dat rows: {nrow(escalc_dat)}"))

  msg_mine(glue("escalc names: {names(escalc_dat %>% select(yi, vi))}"))



  try_mv <-
    tryCatch(
      expr =
        rma.mv(
        yi = yi,
        V = vi,
        slab = study,
        data = escalc_dat,
        random = ~ 1|study/arm # need V for rma.mv
      ),
      error = function(cond) {
        message("mv failed")
      }
    )

  rma <-
    rma(
      yi = yi,
      vi = vi,
      slab = study,
      data = escalc_dat
    )

  rma_mv_try <-
  if (is.null(try_mv)) {
    rma
  } else try_mv

  list(
    rma_mv = rma_mv_try,
    rma = rma
  )

}
