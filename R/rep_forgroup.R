#' Title
#'
#' @param rep_outcome
#' @param rep_timepoint
#' @param rep_intervention
#' @param rep_comparator
#' @param rep_subgroup
#' @param rep_rma_prefix
#' @param rep_pw_mod
#' @param rep_pw_escalc
#'
#' @return
#' @export
#'
#' @examples

rep_forgroup <- function(
  rep_outcome,
  rep_timepoint,
  rep_intervention,
  rep_comparator,
  rep_subgroup,
  rep_rma_prefix,
  rep_pw_mod,
  rep_pw_escalc
){
  ### a little helper function to add Q-test, I^2, and tau^2 estimate info
  mlabfun <- function(text, res) {
    list(bquote(
      paste(
        .(text),
        " (Q = ",
        .(formatC(
          res$QE, digits = 2, format = "f"
        )),
        ", df = ",
        .(res$k - res$p),
        ", p ",
        .(metafor:::.pval(
          res$QEp,
          digits = 2,
          showeq = TRUE,
          sep = " "
        )),
        "; ",
        I ^ 2,
        " = ",
        .(formatC(
          res$I2, digits = 1, format = "f"
        )),
        "%, ",
        tau ^ 2,
        " = ",
        .(formatC(
          res$tau2, digits = 2, format = "f"
        )),
        ")"
      )
    ))
  }


  ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
  ### used to specify in which rows the outcomes will be plotted)
  forest(
    rep_pw_mod,

    # specify subroups
    order = rep_pw_escalc$condition,

    # other args to jiggle
    xlim = c(-12, 5),

    at = log(c(0.25, 0.5, 1, 2, 4)),

    ilab.xpos = c(-7.5,-6,-4,-2.5),

    cex = 1, # 0.75,

    # need to adjust ylim by number of studies/groups
    ylim = c(-1,

             # + 4 to max max rows
             48),
    psize = 1,

    ### 'rows' argument is used to specify in which rows the outcomes will be plotted)
    # number of rows = number of studies in each subgroup
    rows = c(3:15, # ascending rows, I think, bottom group

             # start at max(above) + 5
             20:30,

             # start at max(above) + 5
             35:42),

    # args don't need to jiggle
    atransf = exp,
    ilab = rep_pw_escalc %>%
      select(r, n, r_control, n_control) %>% as.matrix(),
    mlab = mlabfun("RE Model for All Studies", rep_pw_mod),
    header = sprintf("Subgroup analysis by %s", rep_subgroup) %>%
      str_wrap(18),
    main = sprintf(
      "%s %s: %s vs %s",
      timepoint_label(rep_timepoint),
      outcome_label(rep_outcome),
      rep_intervention,
      rep_comparator
    ) %>% str_to_sentence() %>% str_wrap()
  )

  ### set font expansion factor (as in forest() above) and use a bold font
  op <- par(cex = 1, font = 2)

  ### add additional column headings to the plot
  text(
    # copied this from above
    c(-7.5,-6,-4,-2.5),
    # max number of rows
    47,
    c("Rate",
      "Sample size", "Rate", "Sample size"))

  text(c(-6.5,-3.5),
       48,
       c(
         rep_intervention %>% str_to_sentence(),
         rep_comparator %>% str_to_sentence()
       ))


  ### switch to bold italic font
  par(font=4)


  ### add text for the subgroups
  text(
    x = -12,
    # + 2 to max rows
    y = c(17, 32, 44),
    pos = 4,
    labels = rep_pw_escalc$condition %>%
      unique() %>% str_to_sentence())

  ### set par back to the original settings
  par(op)


  ### fit random-effects model in the three subgroups
  res.a <- rma(yi,
               vi,
               subset = (condition == "fibromyalgia"),
               data = rep_pw_escalc)
  res.b <- rma(yi,
               vi,
               subset = (condition == "musculoskeletal"),
               data = rep_pw_escalc)
  res.c <- rma(yi,
               vi,
               subset = (condition == "neuropathic"),
               data = rep_pw_escalc)


  ### add summary polygons for the three subgroups
  addpoly(res.a,
          row = 33.5,
          cex = 1,
          # -1.5 from min of row for subgroup
          mlab = mlabfun("RE Model for Subgroup", res.a))
  addpoly(res.b,
          row = 18.5,
          cex = 1,
          mlab = mlabfun("RE Model for Subgroup", res.b))
  addpoly(res.c,
          row = 1.5,
          cex = 1,
          mlab = mlabfun("RE Model for Subgroup", res.c))

  ### fit meta-regression model to test for subgroup differences
  res <- rma(yi, vi,

             # set subgroup
             mods = ~ condition,


             data = rep_pw_escalc)

  ### add text for the test of subgroup differences
  text(-16, -1.8, pos = 4,
       cex = 1,
       bquote(
         paste(
           "Test for Subgroup Differences: ",
           Q[M],
           " = ",
           .(formatC(
             res$QM, digits = 2, format = "f"
           )),
           ", df = ",
           .(res$p - 1),
           ", p = ",
           .(formatC(
             res$QMp, digits = 2, format = "f"
           ))
         )
       ))

}
