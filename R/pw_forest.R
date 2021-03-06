#' Label a forest plot
#'
#' @param dat that goes into [pw_forest_plot]
#'
#' @return
#' @export

pw_forest_labs <- function(
  outcome,
  timepoint,
  intervention,
  dat,
  mod # must have both returns in
) {

  i_sq <- round(mod$rma$I2, 2)

  tau_sq <- round(mod$rma_mv$tau2, 4)

  total_n <- sum(dat$n) + sum(dat$n_control)

  labs(
    title = glue("{intervention} MA {outcome_label(outcome)}") %>% str_to_sentence(),
    subtitle = glue("{timepoint_label(timepoint)}") %>% str_to_sentence(),
    caption = glue("Participants: {total_n}. Tau-squared: {tau_sq}, I-sq {i_sq}%.")
  )
}

#' Meta-analysis forest plot
#'
#' First group is treatment group, second control.
#'
#' @param mod
#' @param m_type
#' @param dir
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples

pw_forest_plot <- function(mod,
                           dat,
                           m_type,
                           dir,
                           alpha = 0.05
) {


  # fix x axes
  xlims_set <-
    if (m_type == 'lor') c(0, 5) else if (m_type == "smd") c(-2, 5)

  xlims <-
    if (dir == "lower") rev(-1 * xlims_set) else xlims_set
  # remove this later

  aug_ma <-
    mod %>%
    augment() %>%
    clean_names() %>%
    mutate(estimate_type = "study",) %>%
    rename(study = rownames, effect = observed) %>%
    mutate(
      var = mod$vi,
      std_error = sqrt(var),
      weight = 1 / (var + mod$tau2),
      rel_weight = weight / sum(weight)
    )


  tidy_ma <-
    mod %>% tidy() %>%
    clean_names() %>%
    rename(effect = estimate) %>%
    mutate(estimate_type = "synthesised",
           study = "RE model",
           rel_weight = 1
           )

  ci_dat <-
    tidy_ma %>%
    bind_rows(aug_ma) %>%
    mutate(
      lower = effect - qnorm(1 - alpha / 2) * std_error,
      upper = effect + qnorm(1 - alpha / 2) * std_error,
      estimate_type = fct_relevel(estimate_type, "study"),
      m_type = m_type,
      effect_ref = 0
    ) %>%
    arrange(estimate_type)

  transf_dat <- if (m_type == "lor") {
    ci_dat %>% mutate(across(c(effect, lower, upper, effect_ref), exp))
  } else if (m_type == "smd") {
    ci_dat
  }

  # add text labels
  label_dat <-
    transf_dat %>%
    mutate(text_label =
             glue("{round(effect, 2)} [{round(lower,2)}, {round(upper,2)}]")) %>%
    mutate(
      study = if_else(study == "RE model",
                      glue("{as.character(study)} {text_label}"),
                      study
      ),
      study = reorder(study, desc(effect))
    )

  dir_lgl <- dir == "lower"

  # add in class, condition, and ...? dose
  plot_dat <-
    label_dat %>%
    mutate(
      study_mod = study,
      study = str_remove(study, "\\.\\d")
    ) %>%
    left_join(
      dat %>%
        select(study, intervention, condition) %>%
        distinct()
    ) %>%
    mutate(
      condition = if_else(
        str_detect(study, "RE model"),
        "all conditions",
        condition
      ),
      condition = fct_relevel(
        condition,
        "all conditions",
        after = Inf
      ),
      study = fct_reorder(study, effect, .desc = dir_lgl)
    )



  # configure colour palette
  # shift this to argument when I want to customise colours
  dirty_xmas_pal <- list(red = "#b12a1b",
                         green = "#67852e")


  active_pal <- dirty_xmas_pal

  active_pal <-
    if (dir == "lower")
      c(active_pal[2], active_pal[1])
  else
    active_pal


  vlines <- if (m_type == "smd") {
    tibble(
      value = c(0, 0.2,-0.2, 0.5,-0.5, 0.8,-0.8),
      effect = c(
        "no effect",
        rep("small", 2),
        rep("moderate", 2),
        rep("large", 2)
      )
    ) %>%
      filter(value >  min(plot_dat$lower) & value < max(plot_dat$upper)) %>%
      mutate(effect =
               fct_relevel(effect, "no effect", "small", "moderate", "large"))
  } else if (m_type == "lor") {
    tibble(value = 1,
           effect = "no effect")
  }

y_axis_first <-
 levels(plot_dat$study)

y_axis_second <-
  plot_dat %>% arrange(study) %>% pluck("text_label")


  # make plot ---------------------------------------------------------------


  plot_dat %>%
    ggplot() +
    geom_vline(
      aes(xintercept = value,
          linetype = effect),
      data = vlines,
      alpha = 0.6,
      colour = "#13294a",
      size = 0.2
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        y = study,
        yend = study,
        colour = I(
          if_else(lower < effect_ref &
                    upper > effect_ref, "darkgrey", "black")
        )
      ),
      alpha = 0.7,
      size = 1
    ) +
    geom_point(show.legend = FALSE,
               shape = 15,
               aes(
                 x = effect,
                 y = study,
                 size = rel_weight,
                 colour = I(if_else(
                   effect < effect_ref, active_pal[[1]], active_pal[[2]]
                 ))
               )) +

    # add diamond
    geom_point(show.legend = FALSE,
               data = plot_dat %>%
                 filter(str_detect(study, "RE model")),
               shape = 18,
               size = 8,
               aes(
                 x = effect,
                 y = study,
                 colour = I(if_else(
                   effect < effect_ref, active_pal[[1]], active_pal[[2]]
                 ))
               )) +
    facet_grid(estimate_type ~ .,
               scales = "free_y",
               space = "free_y") +
    theme_minimal(
      base_size = 15,
      base_family = "serif"
    ) +
    labs(y = "",
         x = "",
         linetype = "Effect reference") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(angle = 0),
      axis.ticks.y = element_blank(),
      panel.spacing.y = unit(1, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
  # coord_cartesian(xlim = xlims) +
scale_size_continuous(range = c(1, 4))
# +
#     guides(
#       y.sec = ggh4x::guide_axis_manual(
#         breaks = y_axis_first,
#         labels = y_axis_second
#       )
#     )

}




pw_forest_archive <- function(mod_key,
                      font_size = 16) {
  dirty_xmas_pal <- list(red = "#b12a1b",
                         green = "#67852e")

  ma <- mod_key %>% pluck("mod", 1, "rma_mv")

  ma_dat <- mod_key %>% pluck("dat", 1)

  participants <-
    tibble(
      n = sum(ma_dat$n),
      n_comp = sum(ma_dat$n_comp)
    ) %>% mutate(
      total = n + n_comp
    )

  m_type <- mod_key$model_type

  dir <- mod_key$direction_of_improvement

  rma_cap <-
if (mod_key$rma_class == "rma.uni") {"
  Random-effects meta-analysis"} else {"Multi-level random effects meta-analysis"}
  #   if (mod_key$rma_class == "rma.uni") "Multi-level model failed to converge, so a meta-analysis model was fitted instead." else {
  #   glue("Multi-level meta-analysis model: accounts for depedency of within-study variance,
  #   where there are more than one arm (e.g. different dose) for the same intervention in the same study.
  #   Study-level coefficient variation (sigma squared 1): {round(ma$sigma2[[1]], 4)}.
  #   Within-study coefficient variation (sigma squared 2): {round(ma$sigma2[[2]],4)}.")
  # }

  this_title <-
    glue(
      "MA {mod_key$outcome_label}: {mod_key$timepoint_label} {mod_key$type_label}"
    ) %>% str_wrap(40)
  this_subtitle <-
    glue(
      "Interventions: {mod_key$int_1} vs {mod_key$int_2}. Condition: {mod_key$condition}. Class: {mod_key$class}. Dose: {mod_key$dose}"
    ) %>% str_wrap(60)

  this_caption <- glue("{rma_cap}. Participants {participants$total}. I-squared {round(mod_key$i_sq, 2)}%; tau-squared {round(mod_key$tau_squared, 4)}.")

    long_caption <-
       glue(
    "{rma_cap}. Participants {participants$total}. I-squared {round(mod_key$i_sq, 4)}%; tau-squared {round(mod_key$tau_squared, 4)}
#Participants: {mod_key$int_1} {participants$n}; {mod_key$int_2} {participants$n_comp}; total {participants$total}.

    Estimate in direction of improvement ({mod_key$direction_of_improvement}) green, otherwise red.
  95% confidence intervals that estimate an effect are black, those that do not are grey.
  Points are sized by relative variance.

  Ratio of true heterogeneity to total observed variation (I-squared): {round(mod_key$i_sq, 4)}%.
    Estimated variation between studies (tau-squared): {round(mod_key$tau_squared, 4)}.

{rma_cap}") %>% str_wrap(120)

  # wrangle plotdat ---------------------------------------------------------


  aug_ma <-
    ma %>%
    augment() %>%
    clean_names() %>%
    mutate(estimate_type = "study",) %>%
    rename(study = rownames, effect = observed) %>%
    mutate(
      var = ma$vi,
      std_error = sqrt(var),
      weight = 1 / (var + mod_key$tau_squared),
      rel_weight = weight / sum(weight)
    )


 tidy_ma <-
    ma %>% tidy() %>%
    clean_names() %>%
    rename(effect = estimate) %>%
    mutate(estimate_type = "synthesised",
           # ci_lb = ma$ci.lb,
           # ci_ub = ma$ci.ub,
           study = "RE model",
           # study = glue("RE model: {round({effect},2)} [{round({ci_lb},2)}, {round({ci_ub}, 2)}]"),
           rel_weight = 1)


  plot_dat <-
    tidy_ma %>%
    bind_rows(aug_ma) %>%
    mutate(
      lower = effect - qnorm(1 - 0.05 / 2) * std_error,
      upper = effect + qnorm(1 - 0.05 / 2) * std_error,
      estimate_type = fct_relevel(estimate_type, "study"),
      m_type = m_type,
      effect_ref = 0
    ) %>%
    arrange(estimate_type)

  plot_dat <- if (m_type == "lor") {
    plot_dat %>% mutate(across(c(effect, lower, upper, effect_ref), exp))
  } else if (m_type == "smd") {
    plot_dat
  }

  # add text labels
  plot_dat <-
    plot_dat %>%
    mutate(text_label =
             glue("{round(effect, 2)} [{round(lower,2)}, {round(upper,2)}]")) %>%
    mutate(
      study = if_else(study == "RE model",
                      glue("{as.character(study)} {text_label}"),
                      study
                      ),
      study = reorder(study, desc(effect))
    )


  # plot set up -------------------------------------------------------------

  # set xlims
 ci_lims <-
    c(min(plot_dat$lower), max(plot_dat$upper))


  text_x <-
    if_else(dir == "higher",
            ci_lims[2] + (abs(sum(ci_lims)) * 0.1),
            ci_lims[1] - (abs(sum(ci_lims)) * 0.1))


  active_pal <- dirty_xmas_pal

  active_pal <-
    if (dir == "lower")
      c(active_pal[2], active_pal[1])
  else
    active_pal


  vlines <- if (m_type == "smd") {
    tibble(
      value = c(0, 0.2,-0.2, 0.5,-0.5, 0.8,-0.8),
      effect = c(
        "no effect",
        rep("small", 2),
        rep("moderate", 2),
        rep("large", 2)
      ),
      include = if_else(
        value < 0,
        value > ci_lims[1],
        value < ci_lims[2]
      )
    ) %>%
      filter(include) %>%
      mutate(effect =
               fct_relevel(effect, "no effect", "small", "moderate", "large"))
  } else if (m_type == "lor") {
    tibble(value = 1,
           effect = "no effect")
  }


  # make plot ---------------------------------------------------------------


  plot_dat %>%
    mutate(
      text_x = text_x
    ) %>%
    ggplot() +
    geom_vline(
      aes(xintercept = value,
          linetype = effect),
      data = vlines,
      alpha = 0.6,
      colour = "#13294a",
      size = 0.2
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        y = study,
        yend = study,
        colour = I(
          if_else(lower < effect_ref &
                    upper > effect_ref, "darkgrey", "black")
        )
      ),
      alpha = 0.7,
      size = 1
    ) +
    geom_point(show.legend = FALSE,
               shape = 15,
               aes(
                 x = effect,
                 y = study,
                 size = rel_weight,
                 colour = I(if_else(
                   effect < effect_ref, active_pal[[1]], active_pal[[2]]
                 ))
               )) +

    # add diamond
    geom_point(show.legend = FALSE,
               data = plot_dat %>%
                 filter(str_detect(study, "RE model")),
               shape = 18,
               size = 8,
               aes(
                 x = effect,
                 y = study,
                 colour = I(if_else(
                   effect < effect_ref, active_pal[[1]], active_pal[[2]]
                 ))
               )) +

    facet_grid(estimate_type ~ .,
               scales = "free_y",
               space = "free_y") +
    ggthemes::theme_tufte(base_size = font_size) +
    labs(y = "",
         x = "",
         linetype = "Effect reference") +
    theme(
      strip.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.spacing.y = unit(1, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    scale_size_continuous(range = c(1, 4)) +
    labs(title = this_title,
         subtitle = this_subtitle,
         caption = this_caption)

}
