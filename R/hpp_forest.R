#' Label a forest plot
#'
#' @param outcome outcome
#' @param type  ad etc.
#' @param timepoint post_int etc.
#' @param mod nma model
#'
#' @return
#' @export

hpp_forest_labs <- function(outcome, timepoint, mod) {
  n <- sum(mod$network$agd_arm$n)

  tau <-
    mod %>%
    summary() %>%
    as_tibble() %>%
    filter(parameter == "tau") %>%
    pull(mean) %>%
    round(4)

  labs(
    title = glue("{outcome_label(outcome)}") %>%
      str_to_sentence() %>%
      paste("NMA", .) %>%
      str_wrap(),
    subtitle = glue("{timepoint_label(timepoint)}") %>%
      str_to_sentence() %>%
      str_wrap(),
    caption = glue("{n} participants. Tau-sq {tau}.")
  )
}



#' Create a forest plot using mod key inputs
#'
#' @param mod A [multinma] model
#' @param mod_type lor or smd
#' @param dir direction
#'
#' @export
#'

hpp_forest <-
  function(mod,
           mod_type,
           dir,
           font_size = 15,
           this_class = "no class filter") {
    xlims_set <- if (mod_type == 'lor') c(-5, 5) else if (mod_type == "smd") c(-2, 5)

    xlims <- if (dir == "lower") rev(-1 * xlims_set) else xlims_set

    forest_multinma(
      mod,
      model_type = mod_type,
      dir = dir,
      font_size = font_size,
      this_class
    ) +
      facet_grid(class ~ .,
                 scales = "free", space = "free") +
                 # switch = "y") +
                 coord_cartesian(xlim = xlims) +
                   labs(x = "")
  }


hpp_forest_key <- function(mod,
                           mod_key,
                           or = TRUE,
                           font_size = 13,
                           this_class = "no class filter") {
  # set up titles -----------------------------------------------------------
  class_label <-
    if_else(this_class == "no class filter",
            mod_key$class,
            toupper(this_class))

  msg_mine("Set titles")
  this_title <-
    glue("NMA {mod_key$outcome_label}: {mod_key$timepoint_label}") %>%  str_wrap(60)
  this_subtitle <- glue("Condition: {mod_key$condition}. Class: {class_label}. Dose: {mod_key$dose}") %>%
    str_wrap(60)

  this_caption <-
    glue(
      "{mod_key$type_label} compared with {mod_key$trt_ref}. Participants {mod_key$total_participants}."
    ) %>% str_to_sentence()

  long_caption <- glue_col(
    "Participants: {mod_key$total_participants}.

    Estimate in direction of improvement ({mod_key$direction_of_improvement}) green, otherwise red.

  95% credible intervals that estimate an effect are black, those that do not are grey.

  Points indicate mean, sized by total number of participants who contributed to intervention comparison."
  ) %>% str_wrap(120)


  msg_mine("Create plot")

  forest_multinma(mod, mod_key, this_class = this_class, or = or) +
    labs(
      title = this_title,
      subtitle = this_subtitle,
      caption = this_caption,
      y = mod_key$type_label %>% str_to_sentence(),
      x = glue::glue("{mod_key$model_text}") %>% str_to_sentence()
    )  +
    scale_size_continuous("Intervention sample size",
                          guide = guide_legend(nrow = 2)) +
    theme_minimal(base_size = font_size) +
    theme(
      # strip.text.y = element_text(angle = 0),
      text = element_text(family = "serif"),
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(
        colour = "grey",
        linetype = "dotted",
        size = 0.5
      )
      # axis.text.x = element_text(),
      #legend.position = "bottom",
      #legend.direction = "horizontal"
    )


}
