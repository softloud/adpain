#' Intervention results
#'
#' @param nma nma mod results metatab
#'
#'
#' @return
#' @export
#' @family sof
#'
#' @examples

sof_results_dat <- function(
  nma_mod
) {
  # extract intervention estimates
  estimates <-
  nma_mod %>%
    extract_nma_estimates() %>%
    filter(par == "d") %>%
    select(-par) %>%
    rename(intervention = par_index)

  ranks <-
    nma_mod %>%
    posterior_ranks() %>%
      as_tibble() %>%
    clean_names() %>%
      select(intervention = parameter,
             mean,
             sd,
             ci_lb = x2_5_percent,
             ci_ub = x97_5_percent) %>%
      mutate(intervention = intervention %>%
               str_remove("rank\\[") %>% str_remove("\\]")) %>%
    rename_with(~glue("{.x}_rank")) %>%
    rename(intervention = intervention_rank)

  dat <-
  nma_mod %>%
    pluck("network", "agd_arm") %>%
    rename(intervention = .trt) %>%
    group_by(outcome, intervention, timepoint) %>%
    summarise(
      n_studies = n_distinct(.study),
      n_participants = sum(.sample_size)
    ) %>%
    arrange(desc(n_participants), desc(n_studies)) %>%
    full_join(estimates) %>%
    full_join(ranks) %>%
    ungroup()

  totals <-
  dat %>%
    summarise(
      total_studies = sum(n_studies, na.rm = TRUE),
      total_participants = sum(n_participants, na.rm = TRUE)
    )

  # transform data if lor
  m_type <-
    dat$outcome %>%
    unique() %>%
    outcome_mod()

  dat_transf <-
    if(m_type == "lor") {
      dat %>%
        mutate(
          across(c(mean, ci_lb, ci_ub), exp)
        )
    } else if (m_type == "smd") dat # no transf

    # output results
  dat_transf %>%
    mutate(
      # tab_outcome = glue("({n_studies} RCT; {n_participants} participants)"),
      tab_rank = glue("{round(mean_rank)} ({ci_lb_rank} to {ci_ub_rank})"),
      tab_rel = glue("{round(mean, 2)} ({round(ci_lb,2)} to {round(ci_ub, 2)}) ")
    ) %>%
    select(outcome,
           tab_rct = n_studies,
           participants = n_participants,
           intervention, starts_with('tab'), everything()) %>%
    arrange(mean_rank) %>%
    left_join(outcome_key %>% select(-description))
  # %>%
  #   # NB this needs to be updated to total participants
  #   filter(participants > 100)

  # # output table
  # tibble(
  #   n_studies = "todo",
  #   n_participants = "todo",
  #   ci_relative = "todo",
  #   ci_int = "todo",
  #   ci_control = "todo",
  #   difference = "todo",
  #   certainty = "todo",
  #   ci_ranking = "todo",
  #   interpretation = "todo"
  # )

}

#' Table of intervention estimates and ranks
#'
#' @param results_dat from [sof_results_dat]
#'
#' @return
#' @export
#'
#' @examples
sof_results_tab <- function(results_dat){
  results_dat %>%
    # filter(
    #   participants >= 200 |
    #     intervention == "placebo") %>%
    select(
      outcome,
      intervention,
      RCT = tab_rct,
           participants,
           rank = tab_rank,
           rel = tab_rel
           )

}

#' PICO Summary of outcome component
#'
#' @param population
#' @param comparator
#' @param mod
#'
#' @return
#' @export
#'
#' @examples

sof_meta_tab <- function(
  nma_mod,
  interventions,
  population = "adults with chronic pain",
  comparator = "placebo"
) {
  nma_mod %>%
    pluck("network", "agd_arm") %>%
    ungroup() %>%
    summarise(
      outcome = unique(outcome),
      type = unique(type),
      timepoint = unique(timepoint),
      interventions = interventions,
      # this could be buggy; is sample size same in both lor & smd?
      total_participants = sum(.sample_size),
      n_studies = n_distinct(.study)
    ) %>%
    mutate(
      comparator = comparator,
      population = population
    ) %>%
    # eventually move to function
    t() %>%
    as_tibble(rownames = "category") %>%
    rename(
      desc = V1
    ) %>%
    mutate(across(everything(), str_to_sentence))

}

#' SOF
#'
#' Based on [this ref](https://www.sciencedirect.com/science/article/pii/S0895435618303172?casa_token=VOokqa1sWuIAAAAA:pEXsLysLZybSApmf9u9pJ21Ze3J_HZLwktAAjZ-doSZy2l5lG_Fca3PuV-2ul1mM46INPsAhbA). There are three sections: upper, middle and lower.
#'
#' Upper: PICO components + network plot
#' Middle: 8 columns
#' Lower: NMA terminology and abbreviations
#'
#' @param mod
#' @param outcome
#' @param pop
#'
#' @return
#' @export
#'
#' @examples

sof <- function(mod,
                outcome = "antidepressants",
                pop = "adults with chronic pain"
                ) {
  interventions <-
    mod %>%
    pluck("network", "agd_arm") %>%
    select(intervention = .trt) %>%
    filter(intervention != "placebo") %>%
    pull(intervention) %>%
    unique() %>%
    paste(collapse = "; ")



  results <-
    sof_results_tab(mod)

  # meta <-
  #   sof_meta_tab(mod, outcome, interventions) %>% gt()

  # patchwork <- (meta + net_plot) / results
  #
  # patchwork +
  #   patchwork::plot_annotation(
  #     title = "estimates of effects, credible intervals, and certainty of the evidence for {outcome} in {pop}",
  #     subtitle = "Bayesian NMA-SoF table",
  #     caption = "todo"
  #   )

  file <- tempfile()
  net_plot <- plot(mod$network)
  ggsave(file, device = "png")


  results %>%
    gt() %>%
    tab_header(
      title = html("
                   <div class=\"row\">
                   <div class=\"column\">
                   <p> quack </p>
                   </div>
                   <div class = \"column\">
                   <img src \"file\">
                   </div>
                   </div>")
      # title = ggplot_image(net_plot) %>% html()
    )

}


#' Title
#'
#' @param df
#' @param acr
#'
#' @return
#' @export
#'
#' @examples

sof_nnt <- function(df, acr) {
  df %>%
    filter(intervention != "placebo") %>%
    select(outcome, intervention, mean) %>%
    mutate(
      acr = acr,
      rd = map2_dbl(
        mean, acr, calc_rd_par
      ),
      nf_1000 = rd * 1000,
      nf_1000 = ceiling(nf_1000),
      nnt = nnt(mean, acr),
      nnt = ceiling(nnt)
    )


}


# outputs -----------------------------------------------------------------

#' Title
#'
#' @param this_mod
#'
#' @return
#' @export
#'
#' @examples

sof_output <- function(this_mod) {

  # results table
  sof_results_dat(this_mod) %>%
    left_join(rob) %>%
    mutate(high_risk_p = round(high_risk_p, 2)) %>%
    mutate(intervention = str_to_sentence(intervention)) %>%
    select(intervention,
           # type,
           # timepoint,
           RCT = tab_rct,
           participants,
           rel = tab_rel,
           high_risk_p,
           rank = tab_rank) %>%
    kable(
      col.names = c(
        "Intervention",
        # "Type",
        # "Timepoint",
        "RCT",
        "Participants",
        "Relative effect (2)",
        "p[ROB] (3)",
        "Rank (2)"
      )
    ) %>%
    kable_styling() %>%
    add_footnote(
      c("RCT := Number of randomised control trials",
        "Mean estimate with 95 per cent credible interval",
        "p[ROB] := Proportion of high-bias observations"
      ),
      notation = "number"
    ) %>%
    column_spec(4, width = "4cm") %>%
    column_spec(6, width = "3cm")

}

#' Title
#'
#' @param this_mod
#' @param acr
#'
#' @return
#' @export
#'
#' @examples

sof_output_acr <- function(this_mod, acr = 0.31) {

  results <-
    sof_results_dat(this_mod) %>%
    left_join(rob) %>%
    mutate(
      high_risk_p = high_risk_p %>% round(2)
    )


  acr_dat <-
    results %>%
    sof_nnt(acr) %>%
    select(intervention, nf_1000, nnt)

  results %>%
    left_join(acr_dat) %>%
    mutate(nnt = glue("{nnt} of 1000")) %>%
    arrange(nnt) %>%
    select(intervention,
           # type,
           # timepoint,
           RCT = tab_rct,
           participants,
           # nf_1000,
           NNTH = nnt,
           rel = tab_rel,
           high_risk_p,
           rank = tab_rank
    ) %>%
    mutate(intervention = str_to_sentence(intervention)) %>%
    kable(
      col.names = c(
        "Intervention",
        # "Type",
        # "Timepoint",
        "RCT (1)",
        "Participants",
        "NNTH (3)",
        "Relative effect (2)",
        "p[ROB] (4)",
        "Rank (2)"
      )

    ) %>%
    kable_styling() %>%
    add_footnote(
      c("RCT := Number of randomised control trials",
        "Mean estimate with 95 per cent credible interval",
        "NNTH: Number needed to treat to harm",
        "p[ROB] := Proportion of high-bias observations"
      ),
      notation = "number"
    ) %>%
    column_spec(5, width = "4cm") %>%
    column_spec(7, width = "3cm")
}

#' Title
#'
#' @param so
#'
#' @return
#' @export
#'
#' @examples
sof_output_styling <- function(so) {

}

#' Title
#'
#' @param this_mod
#'
#' @return
#' @export
#'
#' @examples

sof_pico <- function(this_mod) {
  this_mod$network$agd_arm %>%
    left_join(timepoint_key %>% select(timepoint, timepoint_label)) %>%
    left_join(type_key %>% select(type, type_label)) %>%
    summarise(
      outcome = unique(outcome),
      outcome_label = unique(outcome_label),
      timepoint = unique(timepoint_label),
      type = unique(type_label) %>%
        paste(collapse = ";") %>% str_remove(";*placebo;*"),
      participants = sum(n),
      studies = n_distinct(.study),
      model = unique(model_text)
    ) %>%
    mutate(
      population = "adults with chronic pain",
      comparator = "placebo",
      improvement = outcome_dir(outcome)
    ) %>%
    select(
      population, outcome_label, improvement, type, timepoint, everything(), comparator
    ) %>%
  #   t() %>%
  #   as_tibble(
  #     rownames = "descriptor") %>%
  #   rename(
  #     details = V1
  #   ) %>%
    mutate(across(everything(), str_to_sentence))
  # %>%
  #   kable(col.names = NULL) %>%
  #   kable_styling()

}
