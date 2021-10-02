#' Pairwise set
#'
#' @param dat Dataframe
#'
#' @export

hpp_rma <- function(dat) {
  m_type <- dat %>% pull(model_type) %>% unique()

  hpp_measure <-
    if (m_type == "smd")
      "SMD"
  else if (m_type == "lor")
    "OR"

  if (m_type == "smd") {
    metafor::escalc(
      measure = hpp_measure,
      m1i = mean,
      m2i = mean_comp,
      sd1i = sd,
      sd2i = sd_comp,
      n1i = n,
      n2i = n_comp,
      slab = study_id,
      data = dat
    )

  } else if (m_type == "lor") {
    metafor::escalc(
      measure = hpp_measure,
      ai = r,
      n1i = n,
      ci = r_comp,
      n2i = n_comp,
      slab = study_id,
      data = dat
    ) %>%
      rma(
        yi = yi,
        vi = vi,
        slab = study_id,
        data = .,
        measure = hpp_measure
      )
  }



}
