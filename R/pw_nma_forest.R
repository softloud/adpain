#' Comparison forest plot of ma and nma
#'
#' @param dat Nma and pw data
#'
#' @return
#' @export
#'
#' @examples

pw_nma_forest <- function(dat) {



  dat %>%
    # order plot by mean
    mutate(
      intervention = fct_reorder(intervention,
                                 mean,
                                 # this is the key thing to flip
                                 # for direction lower
                                 .desc = FALSE
                                 )
    ) %>%
    ggplot() +
    geom_segment(
      aes(x = ci_lb, xend = ci_ub,
          y = intervention,
          yend = intervention,
          #linetype = mod,
          color = mod,
          size = mod,
          alpha = mod
      )
    ) +
    scale_color_discrete(type = c("#2a405c", "#474a40")) +
    #scale_linetype_discrete(c(1,3)) +
    scale_size_discrete(range = c(5,1)) +
    scale_alpha_discrete(range = c(0.6, 0.2)) +
    theme_minimal(
      base_family = "serif"
    ) +
    # facet_grid(
    #   intervention ~ .,
    #   scales = "free_y",
    #   switch = "y"
    # ) +
    # scale_y_discrete(position = "right") +
    theme(
      axis.title.y = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(
      x = "Mean difference from placebo",
      # title = this_title,
      subtitle = "Network meta-analysis vs meta-analysis results",
      color = "Model type",
      linetype = "Model type",
      caption =
        glue("Network meta-analysis (NMA): point estimate [95% credible interval].

             Random-effects meta-analysis (RMA): point estimate [95% confidence interval] I-squared (tau-squared).") %>%
        str_wrap(60)
    )
}
