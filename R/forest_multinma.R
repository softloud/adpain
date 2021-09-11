#' Multinma forest plot
#'
#' @param mod A [multinma] model.
#'
#' @return
#' @export
#'
#' @examples


forest_multinma <- function(mod) {
  lisa_pal <- c(
    blue = "dodgerblue",
    red = "red"
  )
  dirty_xmas_pal <- list(
    red = "#b12a1b",
    green = "#67852e"
  )
  
  active_pal <- dirty_xmas_pal
  
  input_dat <-
    mod %>% 
    pluck("network", "agd_arm") %>% 
    select(study = .study, trt = .trt, n = .sample_size, arm, type) 
  
  placebo_arms <-
    input_dat %>% 
    filter(type == "placebo") %>% 
    group_by(study) %>% 
    summarise(
      placebo_n = sum(n, na.rm = TRUE)
    )
  
  ad_arms <-
    input_dat %>% 
    filter(type == "antidepressant") %>% 
    left_join(placebo_arms, by = "study")
  
  
  sample_sizes <- 
    ad_arms %>% 
    group_by(trt) %>% 
    summarise(
      int_sample = sum(n, na.rm = TRUE) + sum(placebo_n, na.rm = TRUE)
    ) %>% 
    rename(
      intervention = trt
    )
  
  
  
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
    select(intervention, mean, ci_lb, ci_ub)
  
  tau <- stan_dat %>% 
    filter(intervention == "tau") %>% pull(mean)
  
  plot_dat <-
    stan_dat %>% 
    filter(intervention != "tau")
  
  plot_dat %>% 
    mutate(
      intervention = fct_reorder(intervention, mean, desc) 
    ) %>% 
    left_join(
      sample_sizes, by = "intervention"
    ) %>% 
    ggplot() +
    geom_vline(
      xintercept = 0,
      alpha = 0.4,
      linetype = "dotted"
    ) +
    geom_segment(aes(x = ci_lb, xend = ci_ub,
                     y = intervention, yend = intervention,
                     colour = I(
                       if_else(ci_lb < 0 & ci_ub > 0, "grey", "black")
                     ),
    ), alpha = 0.7, size = 1
    ) +
    geom_point(aes(x = mean, y = intervention, colour = 
                     I(if_else(mean < 0, active_pal[[1]], active_pal[[2]])),
                   size = int_sample
    ),
    alpha = 0.85,
    # set to square
    shape = 15 # http://www.sthda.com/english/wiki/ggplot2-point-shapes
    ) +
    geom_text(
      aes(x = max(ci_ub) + abs(max(ci_ub) - min(ci_lb))/8,
          y = intervention,
          label =  glue::glue("{round(mean, 2)} [{round(ci_lb, 2)}, {round(ci_ub, 2)}]")
          ),
      hjust = 1
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
     
}