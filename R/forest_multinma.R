#' Multinma forest plot
#'
#' @param mod A [multinma] model.
#'
#' @return
#' @export
#'
#' @examples


forest_multinma <- function(mod,
                            key,
                            this_class = "no class filter",
                            or = TRUE) {


# set palettes ------------------------------------------------------------


  msg_mine("Set palette according to direction of improvement")

  lisa_pal <- c(
    blue = "dodgerblue",
    red = "red"
  )
  dirty_xmas_pal <- list(
    red = "#b12a1b",
    green = "#67852e"
  )

  active_pal <- dirty_xmas_pal

  active_pal <- if (key$direction_of_improvement == "lower") c(active_pal[2], active_pal[1]) else active_pal


# class filter ------------------------------------------------------------


  mod_dat_all_classes <-
    mod %>%
    # needs to work on model object, not on results
    pluck("network", "agd_arm") %>%
    rename(study = .study, intervention = .trt)

  mod_dat <- if (this_class == "no class filter") {
    mod_dat_all_classes
  } else {
    mod_dat_all_classes %>%
      filter(class == this_class)
  }


# intervention levels -----------------------------------------------------

  int_n <-
    mod_dat %>%
    mutate(across(c(study, intervention), as.character)) %>%
    group_by(intervention, class) %>%
    summarise(
      int_n = sum(n)
    ) %>%
    arrange(desc(int_n))


# wrangle stan ------------------------------------------------------------

  stan_dat <-
    mod %>%
    summary() %>%
    as.data.frame() %>%
    filter(str_detect(parameter, "^d\\[|^tau")) %>%
    mutate(
      parameter = str_remove(parameter, "^d\\[") %>%
        str_remove("\\]")
    ) %>%
    rename(intervention = parameter,
           ci_lb = "2.5%",
           ci_ub = "97.5%"
    ) %>%
    select(intervention, mean, ci_lb, ci_ub) %>%
    filter(intervention %in% mod_dat$intervention)


# convert to odds ratios --------------------------------------------------


  stan_dat <- if(or == TRUE) {
    stan_dat %>%
      mutate(
        model_type = key$model_type,
        mod_ref = 0,
        mod_ref = if_else(model_type == "lor", exp(mod_ref), mod_ref),
        mean = if_else(model_type == "lor", exp(mean), mean),
        ci_lb = if_else(model_type == "lor", exp(ci_lb), ci_lb),
        ci_ub = if_else(model_type == "lor", exp(ci_ub), ci_ub))
  } else stan_dat

  tau <- stan_dat %>%
    filter(intervention == "tau") %>% pull(mean)

  dir_lgl <- key$direction_of_improvement == "lower"


  plot_dat <-
    stan_dat %>%
    filter(intervention != "tau") %>%
    left_join(int_n, by = "intervention") %>%
    mutate(
      intervention = fct_reorder(intervention, mean, .desc = dir_lgl)
    )

  ref_line <- plot_dat$mod_ref %>% unique()

  plot_dat %>%
    ggplot() +
    geom_vline(
      xintercept = ref_line,
      alpha = 0.4,
      linetype = "dotted"
    ) +
    geom_segment(aes(x = ci_lb, xend = ci_ub,
                     linetype = class,
                     y = intervention, yend = intervention,
                     colour = I(
                       if_else(ci_lb < mod_ref & ci_ub > mod_ref, "grey", "black")
                     ),
    ), alpha = 0.95, size = 0.8
    ) +
    geom_point(aes(x = mean,
                   y = intervention,
                   colour = I(if_else(mean < mod_ref, active_pal[[1]], active_pal[[2]])),
                   size = int_n,
                   NULL
    ),
    alpha = 0.85,

    # set to square
    shape = 18 # http://www.sthda.com/english/wiki/ggplot2-point-shapes
    ) +

    # geom_text(
    #   aes(x = max(ci_ub) + abs(max(ci_ub) - min(ci_lb))/8,
    #       y = intervention,
    #       label =  glue::glue("{round(mean, 2)} [{round(ci_lb, 2)}, {round(ci_ub, 2)}]")
    #       ),
    #   size = 4,
    #   hjust = 1
    # ) +


    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )

}
