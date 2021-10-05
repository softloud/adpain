pw_forest <- function(mod_key) {

  ma <- mod_key %>% pluck("mod", 1)

  m_type <- mod_key$model_type

  dir <- mod_key$direction_of_improvement

  this_title <- glue("Outcome {mod_key$outcome_label}: {mod_key$comp_type} comparison")
  this_subtitle <- glue("Interventions: {mod_key$comp}")
  this_caption <- TeX(glue("$\\tau^2 =$ {mod_key$tau_squared}, $I^2 =$ {mod_key$i_squared}."))

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
      rel_weight = weight*effect/sum(weight)
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
           effect_ref = 1
           ) %>%
    arrange(estimate_type)


  plot_dat <- if (m_type == "lor") {
    plot_dat %>% mutate(across(c(effect, lower, upper, effect_ref), exp))
  } else if (m_type == "smd") {plot_dat}


# plot set up -------------------------------------------------------------


  dirty_xmas_pal <- list(
    red = "#b12a1b",
    green = "#67852e"
  )

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
    ggplot() +
    geom_vline(
      aes(
        xintercept = value,
      linetype = effect
      ),
      data = vlines,
      alpha = 0.4,
      colour = "#13294a"
    ) +
    geom_segment(
      aes(x = lower, xend = upper, y = study, yend = study,
          colour = I(
            if_else(lower < 0 & upper > 0, "darkgrey", "black")
          )),
      alpha = 0.95, size = 1.5
    ) +
    geom_point(
      show.legend = FALSE,
      aes(x = effect, y = study, size = rel_weight,
      colour = I(if_else(effect < effect_ref, active_pal[[1]], active_pal[[2]])))
    ) +
    facet_grid(estimate_type ~ ., scales = "free") +
    ggthemes::theme_tufte() +
    labs(
      y = "",
      x = mod_key$model_text
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    scale_size_continuous(range = c(4, 8)) +
    labs(
      title = this_title,
      subtitle = this_subtitle,
      caption = this_caption
    )

}
