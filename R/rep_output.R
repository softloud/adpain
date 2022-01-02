#' @export

get_o_nma <-
  function(this_outcome, this_tp, this_key){
    this_key %>%
      filter(outcome == this_outcome,
             timepoint == this_tp) %>%
      pull(write_path) %>%
      read_rds() %>%
      pluck("result")
  }

#' @export


rep_sof <- function(this_outcome, this_tp) {
  oti_sof %>%
    filter(outcome == this_outcome, timepoint == this_tp) %>%
    select(-outcome, -timepoint) %>%
    ungroup() %>%
    arrange(intervention) %>%
    distinct() %>%
    gt() %>%
    cols_label(
      intervention = "Intervention",
      RCT = "n",
      studies = "p",
      participants = "n",
      rel = "Relative effects",
      nnt = "NNT",
      rank = "Rank",
      high_risk_p = "ROB",
      n = "p"
    ) %>%
    tab_spanner(
      label = "Studies",
      columns = c(RCT, studies)
    ) %>%
    tab_spanner(
      label = "Participants",
      columns = c(participants, n)
    ) %>%
    tab_footnote(
      footnote = "Proportion of total across all interventions.",
      locations = cells_column_labels(
        columns = c(n, studies)
      )
    ) %>%
    tab_footnote(
      footnote = "Mean estimate with 95% credible interval.",
      locations = cells_column_labels(
        columns = c(rel, rank)
      )
    ) %>%
    tab_footnote(
      footnote = "Proportion of observations with at least one high-bias indicator.",
      locations = cells_column_labels(
        columns = high_risk_p
      )
    ) %>%
    tab_footnote(
      footnote = "Estimates of most likely ranking.",
      locations = cells_column_labels(
        columns = rank
      )
    ) %>%
    tab_footnote(
      footnote = "Intervention estimates with total participants < 200 excluded.",
      locations = cells_column_labels(columns = c(intervention, participants))
    ) %>%
    tab_header(subtitle = "Comparison of intervention estimates",
               title = glue("{outcome_label(this_outcome)}") %>%
                 str_to_sentence()
               )

}

#' @export

rep_pw <- function(this_pw) {

  this_model_type  <- this_pw$outcome %>%
    unique() %>%
    outcome_mod()

  this_transf <- if (this_model_type == "lor") exp else NULL

  for(i in 1:nrow(this_pw)) {
    this_mod <- this_pw$pw_rma[[i]]$rma

    tau_sq <- this_mod$tau2
    i_sq <- this_mod$I2

    this_con <- if ("condition" %in%
                    names(this_pw)) {this_pw$condition[[i]]} else ""

    funnel_title <- glue("{this_con}  {timepoint_label(this_pw$timepoint[[i]])}: {this_pw$intervention[[i]]}") %>% str_to_sentence()

    forest_title <- glue("{outcome_label(this_pw$outcome[[i]])} tau2 = {round(tau_sq,4)}, I2 = {round(i_sq,4)}%") %>% str_to_sentence()

    if (this_model_type == "lor") {
      forest(this_mod, main = forest_title, transf = exp)
      funnel(this_mod, main = funnel_title, atransf = exp,
             level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)

    } else {
      forest(this_mod, main = forest_title)
      funnel(this_mod, main = funnel_title,
             level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)

    }

    # if (length(this_pw$regtest[[i]]) != 0) {
    #   summary(this_pw$regtest$result[[i]]) %>%
    #     print()
    #   }

  }

}

#' @export

rep_pico_dat <- function(this_mod){
  ints <-
    this_mod$network$agd_arm %>%
    filter(.trt != "placebo") %>%
    pull(.trt) %>%
    unique() %>%
    paste(collapse = "; ")

    this_mod$network$agd_arm %>%
      left_join(timepoint_key %>% select(timepoint, timepoint_label)) %>%
      left_join(type_key %>% select(type, type_label)) %>%
      summarise(
        outcome_label = unique(outcome_label),
        outcome = unique(outcome),
        timepoint = unique(timepoint_label),
        type = unique(type_label) %>%
          paste(collapse = ";") %>% str_remove(";*placebo;*"),
        participants = sum(n),
        studies = n_distinct(.study),
        model = unique(model_text)
      ) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(
        interventions = ints,
        population = "adults with chronic pain",
        direction = outcome_dir(outcome),
        comparator = "placebo"
      ) %>%
    select(-outcome) %>%
      select(
        population, outcome = outcome_label, type, timepoint, everything(), comparator
      ) %>%
      mutate(across(everything(), str_to_sentence))

  }

#' @export

rep_pico <- function(this_mod){
  # rep_pico_dat(this_mod) %>%
    # kbl(booktabs = T) %>%
    # kable_styling(full_width=FALSE)
    #gt() %>%
    # tab_options(table.width = px(100)
    #
    # )
  rep_pico_dat(this_mod) %>%
    t() %>%
    as_tibble(rownames = "cat") %>%
    mutate(
      cat = if_else(cat == "direction", "Direction of improvement", cat)
    ) %>%
    mutate(across(everything(), str_to_sentence)) %>%
    gt() %>%
    cols_label(cat = "", V1 = "") %>%
    tab_options(
      table.font.size = 11,
      table.width = pct(400))


}

#' @export

pres_pico <- function(this_mod) {
  rep_pico_dat(this_mod) %>%
    t() %>%
    as_tibble(rownames = "cat") %>%
    mutate(
      cat = if_else(cat == "direction", "Direction of improvement", cat)
    ) %>%
    mutate(across(everything(), str_to_sentence)) %>%
    gt() %>%
    cols_label(cat = "", V1 = "") %>%
    tab_options(
      table.font.size = 11,
      table.width = px(400))
}


rep_forest <- function(this_mod, mod_type, dir) {
  hpp_forest(this_mod, mod_type, dir, font_size = 10)
}
