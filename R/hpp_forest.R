#' Create a forest plot using mod key inputs
#' 
#' @param mod A [multinma] model
#' @param mod_key Model key from target
#'
#' @export

hpp_forest <- function(mod, 
                       mod_key, 
                       or = FALSE
                       ){

msg_mine("Model parameters")

print(mod_key)

msg_mine("Set titles")

this_title <- glue("{mod_key$outcome} {mod_key$type} interventions relative to {mod_key$trt_ref}")

print(this_title)
  
this_subtitle <-
  case_when(
  mod_key$target == "m_o_tt" ~ glue("Timepoint: {mod_key$timepoint}"),
  mod_key$target == "m_con_o_tt" ~ glue("Condition: {mod_key$condition} | Timepoint: {mod_key$timepoint}")
)


msg_mine("Create plot")

forest_multinma(mod, mod_key, or) +
  ggthemes::theme_tufte(base_size = 22) +
  labs(
    title = this_title,
    subtitle = this_subtitle,
    caption = 
      glue::glue("Red values indicate intervention had worse effect than {mod_key$trt_ref}.  
Horizontal lines indicate the 95% confidence interval for the effect. 
Points indicate mean, sized by total number of participants who contributed to intervention comparison.
Confidence intervals that do not contain 0 are bold.") %>% 
      str_wrap(120),
    x = glue::glue("Measure of comparison: {mod_key$model_text}"),
    y = mod_key$type
                    )  +
  scale_size_continuous("Intervention sample size") +
  theme(
    axis.text.x = element_text(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) 
  

}
  