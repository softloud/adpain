#' Get set of pw comparisons for a dataset
#'
#' @param this_dat pw data
#' @param this_outcome outcome
#' @param this_timepoint timepoint
#'
#' @return
#' @export
#'
#' @examples

pw_int <-
  function(this_dat,
           this_outcome,
           this_timepoint) {
    all_pw <-
      this_dat %>%
      filter(intervention != "placebo") %>%
      group_by(outcome, timepoint, intervention) %>%
      summarise(n_studies_int = n_distinct(study)) %>%
      arrange(desc(n_studies_int)) %>%
      ungroup() %>%
      mutate(
        dat =
          map(intervention,
              function(intervention) {
                # why has this stopped working?
                get_pw_dat(
                  this_dat,
                  outcome = this_outcome,
                  timepoint = this_timepoint,
                  g1 = intervention,
                  g2 = "placebo"
                )
              }),
        n_dat_obs = map_int(dat, nrow),
        total_participants = map_int(dat, function(df) {
          # wrong!
          sum(df$n) + sum(df$n_control)
        })
      )

    mods <-
      all_pw %>%
      filter(
        n_dat_obs > 2,
        n_studies_int > 2,!is.na(total_participants),
        total_participants > 200
      )

    unviable <-
      all_pw %>%
      anti_join(mods)

    viable <-
      mods %>%
      mutate(
        est_type = "rma",
        mod = map(dat, hpp_rma),
        mean = map_dbl(mod, ~ .x$rma_mv$beta),
        ci_lb = map_dbl(mod, ~ .x$rma_mv$ci.lb),
        ci_ub = map_dbl(mod, ~ .x$rma_mv$ci.ub),
        tau_sq = map_dbl(mod, ~ .x$rma_mv$tau2),
        i_sq = map_dbl(mod, ~ .x$rma$I2)
      )

    bind_rows(viable, unviable)
  }
