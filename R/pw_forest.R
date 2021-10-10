pw_forest <- function(mod_key) {

  dirty_xmas_pal <- list(
    red = "#b12a1b",
    green = "#67852e"
  )

  ma <- mod_key %>% pluck("mod", 1)

  m_type <- mod_key$model_type

  dir <- mod_key$direction_of_improvement

  this_title <- glue("Outcome {mod_key$outcome_label}: {mod_key$timepoint_label} {mod_key$type_label} comparison") %>% str_wrap(60)
  this_subtitle <- glue("Interventions: {mod_key$int_1} vs {mod_key$int_2}. Condition: {mod_key$condition_s}") %>% str_wrap()
  this_caption <- glue_col("
  Estimate in direction of improvement ({mod_key$direction_of_improvement}) green, otherwise red.
  95% confidence intervals that estimate an effect are black, those that do not are grey.

                           Multi-level meta-analysis model: accounts for depency of within-study variance,
                           where there are more than one arm (e.g. different dose) for the same intervention in the same study.
                           Estimated variation between studies (tau-squared): {round(mod_key$tau_squared, 2)}.
                           Study-level coefficient variation (sigma squared 1): {round(ma$sigma2[[1]], 2)}.
                       Within-study coefficient variation (sigma squared 2): {round(ma$sigma2[[2]],2)}.")

# wrangle plotdat ---------------------------------------------------------


  aug_ma <-
  ma %>%
  augment() %>%
    clean_names() %>%
    mutate(
      estimate_type = "study",
    ) %>%
    rename(study = rownames, effect = observed) %>%
    mutate(
      var = ma$vi,
      std_error = var %>% sqrt(),
      weight = 1/(var + mod_key$tau_squared),
      rel_weight = weight/sum(weight)
      )


  tidy_ma <-
  ma %>% tidy() %>%
    clean_names() %>%
    rename(effect = estimate) %>%
    mutate(estimate_type = "synthesised",
           study = "RE model",
           rel_weight = 1)


  plot_dat <-
  tidy_ma %>%
    bind_rows(aug_ma) %>%
    mutate(lower = effect - qnorm(1 - 0.05/2) * std_error,
           upper = effect + qnorm(1 - 0.05/2) * std_error,
           estimate_type = fct_relevel(estimate_type, "study"),
           m_type = m_type,
           effect_ref = 0,
           study = reorder(study,effect),
                      ) %>%
    arrange(estimate_type)


  plot_dat <- if (m_type == "lor") {
    plot_dat %>% mutate(across(c(effect, lower, upper, effect_ref), exp))
  } else if (m_type == "smd") {plot_dat}

  # add text labels
  plot_dat <-
    plot_dat %>%
    mutate(text_label =
             glue("{round(effect, 2)} [{round(lower,2)}, {round(upper,2)}]")
    )


# plot set up -------------------------------------------------------------

  # set xlims
  ci_lims <-
    c(
      quantile(plot_dat$lower, 0.25) - quantile(plot_dat$effect, 0.5) / 2,
      quantile(plot_dat$upper, 0.75) + quantile(plot_dat$effect, 0.5) / 2
    ) %>% as.numeric()

  text_x <-
    if_else(dir == "upper", ci_lims[2] + (sum(ci_lims) * 0.2),
      ci_lims[1] - (sum(ci_lims) * 0.4))


  active_pal <- dirty_xmas_pal

  active_pal <- if (dir == "lower") c(active_pal[2], active_pal[1]) else active_pal


  vlines <- tibble(
    value = c(0, 0.2, -0.2, 0.5, -0.5, 0.8, -0.8),
    effect = c("no effect", rep("small", 2), rep("moderate", 2), rep("large", 2))
  ) %>%
    mutate(
      m_type = m_type,
      value = if_else(m_type == "lor", exp(value), value),
      effect = fct_relevel(effect, "no effect", "small", "moderate", "large")
    )


# make plot ---------------------------------------------------------------


  plot_dat %>%
    mutate(
      text_x = text_x,
      lower = if_else(lower < ci_lims[1], ci_lims[1], lower),
      upper = if_else(upper > ci_lims[2], ci_lims[2], upper)) %>%
    ggplot() +
    geom_vline(
      aes(
        xintercept = value,
      linetype = effect
      ),
      data = vlines,
      alpha = 0.6,
      colour = "#13294a",
      size = 0.2
    ) +
    geom_segment(
      aes(x = lower, xend = upper, y = study, yend = study,
          colour = I(
            if_else(lower < effect_ref & upper > effect_ref, "darkgrey", "black")
          )),
      alpha = 0.7, size = 1
    ) +
    geom_point(
      show.legend = FALSE,
      aes(x = effect, y = study, size = rel_weight,
      colour = I(if_else(effect < effect_ref, active_pal[[1]], active_pal[[2]])))
    ) +
    geom_text(
      aes(
        x = text_x,
        y = study,
        label = text_label
      ),
      size = 2
    ) +
    facet_grid(estimate_type ~ .,
               scales = "free_y",
               space = "free_y") +
    ggthemes::theme_tufte(base_size = 10) +
    labs(
      y = "",
      x = mod_key$model_text,
      linetype = "Effect size"
    ) +
    theme(
      strip.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.spacing.y = unit(2, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    scale_size_continuous(range = c(1, 4)) +
    labs(
      title = this_title,
      subtitle = this_subtitle,
      caption = this_caption
    )

}
