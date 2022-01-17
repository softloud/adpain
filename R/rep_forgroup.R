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

rep_forgroup <- function(rep_outcome,
                         rep_timepoint,
                         rep_intervention,
                         rep_comparator,
                         rep_subgroup,
                         rep_rma_prefix,
                         rep_pw_escalc,
                         this_subgroup,
                         a_levels,
                         b_levels,
                         c_levels,
                         a_label = "A group",
                         b_label = "B group",
                         c_label = "C group") {
  # set plot bits -----------------------------------------------------------


  # fit model ---------------------------------------------------------------
  this_mod <- outcome_mod(rep_outcome)

  rep_pw_mod <- if (this_mod == "lor") {
    escalc(
      ai = r,
      ci = r_control,
      n1i = n,
      n2i = n_control,
      data = rep_pw_escalc,
      measure = "OR",
      slab = study
    ) %>% rma(
      yi = yi,
      vi = vi,
      slab = study,
      measure = "OR",
      data  = .
    )
  } else {
    rma(
      yi = smd,
      vi = se_smd,
      slab = study,
      data = rep_pw_escalc,
      measure = "SMD"
    )
  }

  rep_pw_escalc <- if (this_mod == "smd") {
    rep_pw_escalc %>%
      rename(yi = smd,
             vi = se_smd)
  } else {rep_pw_escalc}

  # define levels
  c_levels <- ifelse(is.na(c_label), 0, c_levels)

  row_list <-
    if (c_levels > 0) {
      list(
        c = (3:(2 + c_levels)),
        # ascending rows, I think, bottom group

        # start at max(above) + 5
        b = (2 + c_levels + 5):(2 + c_levels + 5 + b_levels - 1),

        # start at max(above) + 5
        a = (2 + c_levels + 5 + b_levels - 1 + 5):(2 + c_levels + 5 + b_levels -
                                                     1 + 5 + a_levels - 1)
      )

    } else {
      list(
        # start at max(above) + 5
        b = (2 + c_levels + 5):(2 + c_levels + 5 + b_levels - 1),

        # start at max(above) + 5
        a = (2 + c_levels + 5 + b_levels - 1 + 5):(2 + c_levels + 5 + b_levels -
                                                     1 + 5 + a_levels - 1)
      )

    }
  # identify maximum rows
  max_rows <-
    sum(a_levels, b_levels, c_levels, na.rm = TRUE) + # add levels
    5 * 3

  # set xlimit (i.e., where the text of the authors' names are)
  this_xlim <- -16

  subgroup_labels <- if (c_levels == 0) {
    c(a_label, b_label)
  } else {
    c(a_label, b_label, c_label)
  }


  subgroup_order <- rep_pw_escalc %>% pull(main_aim)
  # deal with differences between smd and lor -------------------------------

  ilab_matrix <-
    if (outcome_mod(rep_outcome) == "lor") {
      rep_pw_escalc %>%
        select(r, n, r_control, n_control) %>% as.matrix()
    } else {
      rep_pw_escalc %>%
        select(mean, sd, n, mean_control, sd_control, n_control) %>%
        mutate(across(c(contains("mean"), contains("sd")), round, 2)) %>%
        as.matrix()
    }

  ilab_pos <-
    if (outcome_mod(rep_outcome) == "lor") {
      c(-7.5,-6,-4,-2.5)
    } else {
      c(-8,-7,-6,-5,-4,-3)
    }

  ilab_headers <-
    if (outcome_mod(rep_outcome) == "lor") {
      c("Rate",
        "N", "Rate", "N")
    } else {
      rep(c("Mean", "SD", "N"), 2)
    }

  ilab_spanners <-
    if (outcome_mod(rep_outcome) == "lor") {
      c(-7.5,-4)
    } else {
      c(-8, -5)
    }





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
    order = subgroup_order,

    # other args to jiggle

    ### Change this for cpsi!
    atransf = exp,
    ###

    xlim = c(-12, 5),

    at = log(c(0.25, 0.5, 1, 2, 4)),

    ilab = ilab_matrix,

    ilab.xpos = ilab_pos,

    cex = 1,
    # 0.75,

    # need to adjust ylim by number of studies/groups
    ylim = c(-1,
             max_rows),
    psize = 1,

    ### 'rows' argument is used to specify in which rows the outcomes will be plotted)
    # number of rows = number of studies in each subgroup
    rows = row_list %>% unlist(),
    # args don't need to jiggle



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
    this_xlim,
    max_rows - 1,
    sprintf("Subgroup analysis by %s", rep_subgroup) %>% str_wrap(20),
    adj = 0
  )

  text(# copied this from above
    ilab_pos - 0.25,
    # max number of rows
    max_rows - 1,
    ilab_headers,
    adj = 0)

  text(
    x = ilab_spanners - 0.25,
    y = max_rows,
    labels = c(
      rep_intervention %>% str_to_sentence(),
      rep_comparator %>% str_to_sentence()
    ),
    adj = 0
  )

  text(
    log(0.5),
    max_rows - 1,
    sprintf("%s is better", outcome_dir(rep_outcome)) %>% str_to_sentence(),
    adj = 0
  )

  ### switch to bold italic font
  par(font = 4)


  ### add text for the subgroups
  text(
    x = this_xlim + 4,
    # + 2 to max rows
    y = c(max(row_list$a) + 1,
          max(row_list$b) + 1,
          max(row_list$c) + 1),
    pos = 4,
    labels = subgroup_labels %>% str_to_sentence()
  )

  ### set par back to the original settings
  par(op)


# fit subgroup random effects models --------------------------------------



  ### fit random-effects model in the three subgroups
  res.a <- rma(yi,
               vi,
               subset = (main_aim == a_label),
               data = rep_pw_escalc)
  res.b <- rma(yi,
               vi,
               subset = (main_aim == b_label),
               data = rep_pw_escalc)
  if (c_levels != 0) {
    res.c <- rma(yi,
                 vi,
                 subset = (main_aim == c_label),
                 data = rep_pw_escalc)

  }


  ### add summary polygons for the three subgroups
  addpoly(
    res.a,
    row = min(row_list$a) - 1.5,
    cex = 1,
    # -1.5 from min of row for subgroup
    mlab = mlabfun("RE Model for Subgroup", res.a)
  )
  addpoly(
    res.b,
    row = min(row_list$b) - 1.5,
    cex = 1,
    mlab = mlabfun("RE Model for Subgroup", res.b)
  )

  if (c_levels > 0) {
    addpoly(
      res.c,
      row = 1.5,
      cex = 1,
      mlab = mlabfun("RE Model for Subgroup", res.c)
    )
  }
  ### fit meta-regression model to test for subgroup differences
  res <- rma(yi, vi,

             # set subgroup
             mods = ~ main_aim,


             data = rep_pw_escalc)

  ### add text for the test of subgroup differences
  text(this_xlim + 5,
       -1.75,
       pos = 4,
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
