#' Pairwise set
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
      m2i = mean_comp,
      sd1i = sd,
      sd2i = sd_comp,
      n1i = n,
      n2i = n_comp,
      slab = study,
      data = dat
    )
  } else if (m_type == "lor") {
    metafor::escalc(
      measure = hpp_measure,
      ai = r,
      n1i = n,
      ci = r_comp,
      n2i = n_comp,
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

  if (is.null(try_mv)) {
    rma(
      yi = yi,
      vi = vi,
      slab = study,
      data = escalc_dat
    )
  } else try_mv

}
