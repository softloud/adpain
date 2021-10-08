#' Create a forest plot using mod key inputs
#'
#' @param mod A [multinma] model
#' @param mod_key Model key from target
#'
#' @export

hpp_forest <- function(mod,
                       mod_key,
                       or = TRUE
                       ){

msg_mine("Model parameters")

print(mod_key)

msg_mine("Set titles")
this_title <- glue("Outcome {mod_key$outcome_label}: {mod_key$timepoint_label} {mod_key$type_label} comparison") %>% str_wrap()
this_subtitle <- glue("Condition: {mod_key$condition_s}") %>% str_wrap()
this_caption <- glue_col("
  Estimate in direction of improvement ({mod_key$direction_of_improvement}) green, otherwise red.
  95% confidence intervals that estimate an effect are black, those that do not are grey.
  Points indicate mean, sized by total number of participants who contributed to intervention comparison.")


msg_mine("Create plot")

forest_multinma(mod, mod_key, or) +
  ggthemes::theme_tufte(base_size = 15) +
  labs(
    title = this_title,
    subtitle = this_subtitle,
    caption = this_caption,
    x = glue::glue("Measure of comparison: {mod_key$model_text}"),
    y = mod_key$type
                    )  +
  scale_size_continuous("Intervention sample size",
                        guide = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


}
