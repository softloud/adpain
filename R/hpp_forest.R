#' Create a forest plot using mod key inputs
#' 
#' @param mod A [multinma] model
#' @param mod_key Model key from target
#' @param mod_index Specific index of model for key
#'
#' @export

hpp_forest <- function(mod, mod_key, mod_index){

mod_key <- 
  mod_key %>% filter(model_index == mod_index)

forest_multinma(mod) +
  ggthemes::theme_tufte(base_size = 16) +
  labs(
    title = glue("Relative effects of antidepressant against placebo for {mod_key$outcome}"),
    subtitle = glue("Condition: {mod_key$condition}"),
    caption = glue("Lower scores (blue) favour intervention, higher scores (red) favour placebo; i.e., 
                   blue indicates intervention caused a decrease in pain. Horizontal lines indicate the 95% confidence interval for the effect. Points indicate mean, sized by total number of participants who contributed to intervention comparison.
                   Confidence intervals that do not contain 0 are bold.") %>% 
      str_wrap(80),
    x = glue("Measure of comparison: {mod_key$model_text}"),
    y = "Antidepressant"
                    )  +
  scale_size_continuous("Intervention sample size")
  

}
  