#' Create a forest plot using mod key inputs
#' 
#' @param mod A [multinma] model
#' @param mod_key Model key from target
#' @param mod_index Specific index of model for key
#'
#' @export

hpp_forest <- function(mod, 
                       mod_key, 
                       mod_index,
                       subtitle = "SET SUBGROUPS"){

message("--] Selecting model key parameters")
this_mod_key <- 
  mod_key %>% filter(index == mod_index)

print(this_mod_key)


message("--] Create plot")

forest_multinma(mod) +
  ggthemes::theme_tufte(base_size = 20) +
  labs(
    title = glue("Relative effects of antidepressant for {this_mod_key$outcome}"),
    subtitle = subtitle,
    caption = 
      glue::glue("Red values indicate intervention had worse effect than [SET REF TREATMENT].  
Horizontal lines indicate the 95% confidence interval for the effect. 
Points indicate mean, sized by total number of participants who contributed to intervention comparison.
Confidence intervals that do not contain 0 are bold.") %>% 
      str_wrap(120),
    x = glue::glue("Measure of comparison: {this_mod_key$model_text}"),
    y = "SET INTERVENTION TYPE"
                    )  +
  scale_size_continuous("Intervention sample size") +
  theme(
    axis.text.x = element_text(),
    legend.position = "bottom",
    legend.direction = "horizontal"
 
  )
  

}
  