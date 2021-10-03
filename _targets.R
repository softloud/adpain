# pkgs --------------------------------------------------------------------
suppressMessages({
  library(targets)
  library(tidyverse)
  library(tarchetypes)
  library(janitor)
  library(glue)
  # library(dontpanic)
  library(gt)
  library(assertthat)
  library(rmarkdown)
  library(multinma)
  # library(hppapp) # should be calling from this app
  library(metafor)
  library(varameta)

  conflicted::conflict_prefer("filter", "dplyr")

})

installed_pkg <- installed.packages()[, 1]

if ("dontpanic" %in% installed_pkg) {
  require(dontpanic)
}


# functions ---------------------------------------------------------------
list.files("R", full.names = TRUE)  %>%
  map(source)

# set iter > 2000 for final run
safe_nma <- safely(nma, otherwise = "failed")


# set up targets ----------------------------------------------------------

tar_option_set(packages = "dplyr")
options(mc.cores = parallel::detectCores() - 1)


# buffer limit reached ----------------------------------------------------

# run this
Sys.setenv("VROOM_CONNECTION_SIZE" = 2 * 131072)


# begin targets -----------------------------------------------------------

list(
  # raw data ----------------------------------------------------------------

  tar_target(
    r_obs_dat,
    read_csv("data/obs_dat-2021-10-03 15:07:10.csv") %>%
      clean_names() %>%
      mutate(gs_row = row_number() + 1) %>%
      select(gs_row, everything())
  ),

  tar_target(
    r_outcome_key,
    read_csv("data/outcome-2021-10-03 15:15:15.csv") %>%
      clean_names() %>%
      mutate(
        outcome = fct_relevel(outcome, "mood_overall", "mood_depression", "mood_anxiety")
      )

  ),


  # wrangle outcome key -----------------------------------------------------

  tar_target(
    w_outcome_key,
    r_outcome_key %>%
      mutate(
        outcome = fct_relevel(
          outcome,
          "pain_int",
          "pain_sub",
          "pain_mod",
          "mood",
          "physical",
          "sleep",
          "qol",
          "adverse",
          "withdrawal",
          "serious_adverse",
          "adverse_dropout",
          "adverse_number"
        ),
        outcome_nma = fct_reorder(outcome_nma, outcome, first),
        outcome_label = fct_reorder(outcome_label, outcome_nma, first)
      )
  ),
  # wrangle obs dat ---------------------------------------------------------

  tar_target(
    w_obs_calc,
    r_obs_dat %>%
      mutate(across(where(is.character), tolower)) %>%
      mutate(
        # convert r percent to proportion
        r_percent = r_percent / 100,
        # get n from r and r percent
        n = if_else(is.na(n) & r > 0 & r_percent > 0,
                    round(r / r_percent),
                    n) %>% as.integer(),

        # get r from n and r percent
        r = if_else(is.na(r) & r_percent > 0 & n > 0,
                    round(r_percent * n),
                    r) %>% as.integer(),

        # estimate mean & se from median and iqr
        mean = if_else(
          is.na(mean) &
            !is.na(median) & !is.na(iqr_lower) & !is.na(iqr_higher),
          varameta::wan_mean_C3(q_1 = iqr_lower,
                                m = median,
                                q_3 = iqr_higher),
          mean
        ),

        se = if_else(
          is.na(se) & is.na(sd) &
            !is.na(median) &
            !is.na(iqr_lower) & !is.na(iqr_higher) & !is.na(n),
          varameta::wan_se_C3(
            q_1 = iqr_lower,
            m = median,
            q_3 = iqr_higher,
            n = n
          ),
          se
        ),

        # calculate sd & se
        sd = if_else(
          is.na(se) & is.na(sd) & !is.na(mean) & !is.na(n) & !is.na(ci_upper),
          sqrt(n) * (ci_upper - mean) / qnorm(1 - 0.05 / 2) ,
          sd
        ),
        sd = if_else(
          is.na(se) & is.na(sd) & !is.na(mean) & !is.na(n) & !is.na(ci_lower),
          sqrt(n) * (mean - ci_lower) / qnorm(1 - 0.05 / 2),
          sd
        ),
        sd = if_else(se > 0 & n > 0 & is.na(sd),
                     se * sqrt(n),
                     sd),
        se = if_else(sd > 0 & n > 0 & is.na(se),
                     sd / sqrt(n),
                     se)
      )
  ),

  tar_target(
    w_obs_type,
    w_obs_calc %>%
      mutate(
        intervention_category =
          case_when(
            intervention_grouping == "antidepressant" ~ "ad",
            intervention_grouping == "non-antidepressant pharmacological" ~ "non_ad_pharm",
            intervention_grouping == "non-pharmacological" ~ "non_pharm",
            intervention_grouping == "placebo" ~ intervention_grouping
          )
      )
  ),

  tar_target(
    w_obs_outcome,
    w_obs_type %>%
      left_join(w_outcome_key,
                by = "outcome")
  ),


  # mood hierarchy ----------------------------------------------------------

  tar_target(
    w_mood_hierarchy,
    w_obs_outcome %>%
      filter(str_detect(outcome, "mood")) %>%
      group_by(study_id, arm, timepoint) %>%
      summarise(outcome = unique(outcome) %>% paste(collapse = ";")) %>%
      mutate(
        outcome = str_split(outcome, pattern = ";"),
        n_outcomes = map_int(outcome, length)
      ) %>%
      filter(n_outcomes > 1) %>%
      unnest(outcome) %>%
      ungroup() %>%
      arrange(desc(n_outcomes), study_id, arm, timepoint, outcome)
  ),

  tar_target(
    w_mood_exclude,
    w_mood_hierarchy %>%
      mutate(
        outcome = fct_relevel(outcome, "mood_overall",
                              "mood_depression",
                              "mood_anxiety")
      ) %>%
      group_by(study_id, arm, timepoint) %>%
      arrange(outcome) %>%
      filter(outcome != first(outcome)) %>%
      ungroup() %>%
      select(-n_outcomes)
  ),


  # all data wrangled -------------------------------------------------------

  tar_target(
    obs_excluded,
    w_obs_outcome %>%
      mutate(
        exclusion_reason = case_when(
          sd < 0 ~ "negative sd",
          se < 0 ~ "negative se",
          model_type == "smd" &
            (is.na(sd) | is.na(mean) | is.na(n)) ~
            "smd_missing",
          model_type == "lor" &
            (is.na(r) | is.na(n)) ~ "lor_missing",
          n < 1 ~ "Sample size less than 1",
          r > n ~ "r > n",
          is.na(outcome) ~ "outcome not specified"
        )
      ) %>%
      filter(!is.na(exclusion_reason)) %>%
      select(exclusion_reason, everything())
  ),




  tar_target(
    obs_dat,
    w_obs_outcome %>%
      anti_join(obs_excluded) %>%
      anti_join(w_mood_exclude) %>%
      ungroup()  %>%
      mutate(
        outcome_all = outcome,
        outcome = outcome_nma,
        time_no_change = timepoint,
        timepoint = if_else(change_score == TRUE, # isTRUE(change_score) bad
                            "change_score",
                            timepoint)

      )
  ),


  # subgroups ---------------------------------------------------------------

  tar_target(
    subgroups,
    obs_dat %>%
      filter(
        outcome != "adverse_dropout",
        outcome != "adverse_number",
        outcome != "serious_adverse",
        intervention_grouping != "placebo",
        timepoint != "baseline"
      ) %>%
      select(
        outcome,
        intervention_grouping,
        timepoint,
        general_pain_grouping,
        ad_class
      ) %>%
      distinct()
  ),

  tar_target(
    subgroup_type,
    subgroups %>%
      select(outcome, intervention_grouping, timepoint) %>%
      group_by(outcome, intervention_grouping, timepoint) %>%
      distinct() %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_condition,
    subgroups %>%
      group_by(outcome_nma, intervention_category, general_pain_grouping) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_condition_class,
    subgroups %>%
      group_by(
        outcome_nma,
        intervention_category,
        general_pain_grouping,
        ad_class
      ) %>%
      tar_group(),
    iteration = "group"
  ),


  # model data filtered -----------------------------------------------------



  # models ------------------------------------------------------------------
  tar_target(m_key_fn,
             function(mod, mod_tar) {
               mod %>%
                 pluck("result", "network", "agd_arm") %>%
                 filter(type != "placebo") %>%
                 summarise(
                   outcome = unique(outcome),
                   timepoint = unique(timepoint),
                   type = unique(type),
                   model_type = unique(model_type),
                   trt_ref = mod$result$network$treatments[[1]]
                 ) %>%
                 mutate(target = mod_tar) %>%
                 select(target, everything()) %>%
                 unite(filename, everything(), sep = "-", remove = FALSE) %>%
                 mutate(
                   netpath = glue("images/net/{filename}.png"),
                   forestpath = glue("images/forest/{filename}.png"),
                   pwpath = glue("images/pw/{filename}.png")
                 ) %>%
                 left_join(r_outcome_key, by = c("outcome", "model_type")) %>%
                 select(outcome, everything(), contains("path"))
             }),

  # outcome, timepoint, type ------------------------------------------------


  tar_target(
    m_o_tt_dat,
    obs_dat %>%
      filter(type != "placebo") %>%
      group_by(outcome, timepoint, type) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_o_tt,
    hpp_nma(m_o_tt_dat, obs_dat),
    pattern = map(m_o_tt_dat),
    iteration = "list"
  ),

  tar_target(m_o_tt_key,
             if (!is.null(m_o_tt$error)) {
               tibble(target = "m_o_tt", )
             } else {
               m_key_fn(m_o_tt, "m_o_tt")
             }
             ,
             pattern = map(m_o_tt)),


  # models by condition -----------------------------------------------------


  # condition: pain_sub -----------------------------------------------------



  tar_target(
    m_con_pain_sub_dat,
    obs_dat %>%
      filter(type != "placebo",
             outcome == "pain_sub") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_pain_sub,
    hpp_nma(m_con_pain_sub_dat, obs_dat),
    pattern = map(m_con_pain_sub_dat),
    iteration = "list"
  ),

  tar_target(m_con_pain_sub_key,
             if (!is.null(m_con_pain_sub$error)) {
               tibble(target = "m_con_pain_sub", )
             } else {
               m_key_fn(m_con_pain_sub, "m_con_pain_sub") %>%
                 mutate(
                   condition =
                     m_con_pain_sub %>%
                     pluck("result", "network", "agd_arm") %>%
                     filter(!is.na(condition_general)) %>%
                     pull(condition_general) %>% unique()
                 )
             }
             ,
             pattern = map(m_con_pain_sub)),


  # condition: mood ---------------------------------------------------------




  tar_target(
    m_con_mood_dat,
    obs_dat %>%
      filter(type != "placebo",
             outcome == "mood") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_mood,
    hpp_nma(m_con_mood_dat, obs_dat),
    pattern = map(m_con_mood_dat),
    iteration = "list"
  ),

  tar_target(m_con_mood_key,
             if (!is.null(m_con_mood$error)) {
               tibble(target = "m_con_mood", )
             } else {
               m_key_fn(m_con_mood, "m_con_mood") %>%
                 mutate(
                   condition =
                     m_con_mood %>%
                     pluck("result", "network", "agd_arm") %>%
                     filter(!is.na(condition_general)) %>%
                     pull(condition_general) %>% unique()
                 )
             }
             ,
             pattern = map(m_con_mood)),



  # condition: adverse ------------------------------------------------------

  tar_target(
    m_con_adverse_dat,
    obs_dat %>%
      filter(type != "placebo",
             outcome == "adverse") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_adverse,
    hpp_nma(m_con_adverse_dat, obs_dat),
    pattern = map(m_con_adverse_dat),
    iteration = "list"
  ),

  tar_target(m_con_adverse_key,
             if (!is.null(m_con_adverse$error)) {
               tibble(target = "m_con_adverse", )
             } else {
               m_key_fn(m_con_adverse, "m_con_adverse") %>%
                 mutate(
                   condition =
                     m_con_adverse %>%
                     pluck("result", "network", "agd_arm") %>%
                     filter(!is.na(condition_general)) %>%
                     pull(condition_general) %>% unique()
                 )
             }
             ,
             pattern = map(m_con_adverse)),

  # wrangle model keys ------------------------------------------------------

  tar_target(
    m_key,
    list(
      m_o_tt_key,
      m_con_pain_sub_key,
      m_con_mood_key,
      m_con_adverse_key
    ) %>%
      map_df(bind_rows) %>%
      group_by(target) %>%
      mutate(index = 1:n()) %>%
      select(target, index, everything()) %>%
      filter(!is.na(outcome)) %>%
      ungroup() %>%
      mutate(plot_index = row_number())

  ),

  # pairwise ----------------------------------------------------------------

  # get combinations
  tar_target(
    pw_type_combn,
    subgroup_type %>%
      ungroup() %>%
      inner_join(obs_dat) %>%
      pull(intervention) %>%
      unique() %>%
      c("placebo", .) %>%
      combn(2) %>% {
        tibble(int_1 = .[1, ],
               int_2 = .[2, ])
      } %>%
      bind_cols(subgroup_type) %>%
      mutate(comp = glue("{int_1}_{int_2}")) %>%
      rename(comp_grouping = intervention_grouping)
    ,
    pattern = map(subgroup_type)
  ),

  tar_target(
    pw_type_study,
    obs_dat %>%
      filter(
        outcome == subgroup_type$outcome,
        intervention_grouping %in% c(subgroup_type$intervention_grouping, "placebo")
      ) %>%
      select(
        outcome,
        intervention_grouping,
        study_id,
        intervention,
        timepoint
      ) %>%
      mutate(subgroup = subgroup_type$tar_group) %>%
      group_by(outcome, study_id, subgroup, timepoint) %>%
      summarise(interventions = unique(intervention) %>% paste(collapse = ";")) %>%
      mutate(
        comp_grouping = subgroup_type$intervention_grouping,
        int_split = str_split(interventions, ";"),
        n_int = map_int(int_split, length)
      )
    ,
    pattern = map(subgroup_type)
  ),

  tar_target(
    pw_type_match,
    pw_type_combn %>%
      full_join(pw_type_study) %>%
      mutate(match = pmap_lgl(list(
        int_1, int_2, int_split
      ),
      function(i1, i2, i) {
        i1 %in% i & i2 %in% i
      })) %>%
      select(int_1, int_2, int_split, match, everything()) %>%
      filter(match)
  ),

  tar_target(
    pw_type_group,
    pw_type_match %>%
      group_by(outcome, comp_grouping, interventions, timepoint) %>%
      summarise(
        n_studies = n_distinct(study_id),
        studies = unique(study_id) %>% paste(collapse = ";")
      ) %>%
      filter(n_studies > 1) %>%
      arrange(desc(n_studies)) %>%
      mutate(study_id = str_split(studies, ";")) %>%
      unnest(study_id) %>%
      group_by(outcome, comp_grouping, interventions, timepoint) %>%
      select(outcome, comp_grouping, interventions, study_id, everything()) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    pw_type_wide,
    {
      int_dat <-
        pw_type_group %>%
        mutate(intervention = str_split(interventions, ";")) %>%
        unnest(intervention) %>%
        left_join(obs_dat,
                  by = c("outcome", "study_id", "intervention", "timepoint")) %>%
        group_split(intervention)

      int_dat %>%
        pluck(2) %>%
        select(outcome,
               comp_grouping,
               study_id,
               intervention,
               arm,
               mean,
               sd,
               n,
               r,
               timepoint) %>%
        rename_with(~ glue("{.x}_comp"),
                    any_of(c(
                      "arm", "mean", "sd", "n", "r", "intervention"
                    ))) %>%
        full_join(int_dat[[1]],
                  by = c("outcome", "comp_grouping", "study_id", "timepoint")) %>%
        select(
          model_type,
          outcome,
          timepoint,
          comp_grouping,
          study_id,
          starts_with("arm"),
          starts_with("mean"),
          starts_with("sd"),
          starts_with('n'),
          starts_with("r"),
          starts_with("intervention")
        ) %>%
        select(-r_percent)
    },
    pattern = map(pw_type_group),
    iteration = "list"
  ),

  tar_target(
    pw_type_ma,
    pw_type_wide %>%
      hpp_rma(),
    pattern = map(pw_type_wide),
    iteration = "list"

  ),


  # network plots -----------------------------------------------------------
  tar_target(plot_net, {
    msg_mine("Set up title by outcome, type, timepoint")
    this_title <-
      glue("Direct evidence for {m_key$outcome}")

    this_title %>% print()

    msg_mine("Target:")
    m_key$target %>% print()

    msg_mine("Index:")
    m_key$index %>% print()

    msg_mine("Subtitle:")
    this_subtitle <- case_when(
      m_key$target == "m_o_tt" ~
        glue(
          "Subgroups: timepoint [{m_key$timepoint}] and type [{m_key$type}]"
        ),
      str_detect(m_key$target, "^m_con") ~
        glue(
          "Subgroups: timepoint [{m_key$timepoint}], type [{m_key$type}], condition [{m_key$condition}]"
        )
    )

    this_subtitle %>% print()

    mod <-
      if (m_key$target == "m_o_tt") {
        m_o_tt[[m_key$index]]
      } else if (m_key$target == "m_con_pain_sub") {
        m_con_pain_sub[[m_key$index]]
      } else if (m_key$target == "m_con_mood") {
        m_con_mood[[m_key$index]]
      } else if (m_key$target == "m_con_adverse") {
        m_con_adverse[[m_key$index]]
      }

    msg_mine("Create plot")
    mod$result$network %>%
      plot() +
      labs(subtitle = this_subtitle,
           title = this_title)

  },
  pattern = map(m_key)),

  tar_target(plot_net_write,
             {
               glue(
                 " Writing net plot for m_o_tt:
                        outcome {m_key$outcome}
                        timepoint {m_key$timepoint}
                        type {m_key$type}
                        condition {m_key$condition}"
               ) %>% msg_mine()

               msg_mine(m_key$netpath)
               ggsave(
                 here::here("bksite",
                            m_key$netpath),
                 plot_net,
                 width = 11,
                 height = 6
               )
             },
             pattern = map(m_key, plot_net)),


  # forest ------------------------------------------------------------------


  # forest: generic ---------------------------------------------------------

  # this is a check that forest_multinma works
  tar_target(plot_forest_generic, {
    msg_mine("Select an arbitrary model")

    this_mod <-
      m_o_tt %>%
      pluck(1, "result")

    key <- m_key %>%
      filter(target == "m_o_tt", index == 1)

    # print(summary(this_mod))

    msg_mine("Identify the dataframe req for conf ints text")
    # this_mod

    msg_mine("Plot generic forest")

    forest_multinma(this_mod, key)
  }),


  # forest: dev -------------------------------------------------------------
  tar_target(plot_forest_dev, {
    # select an arbitrary lor model
    m_o_tt_key_row <-
      m_key %>%
      filter(outcome == "mood", target == "m_o_tt") %>%
      head(1)

    msg_mine(" Selected row from m_model_key")
    print(m_o_tt_key_row)

    msg_mine(" Get model")
    mod <-
      m_o_tt %>%
      pluck(m_o_tt_key_row$index) %>%
      pluck("result")

    msg_mine(" Create plot")
    hpp_forest(mod,
               m_o_tt_key_row)

  }),

  tar_target(
    plot_write_forest_dev,
    ggsave(
      here::here("bksite", "images", "dev", "forest.png"),
      plot_forest_dev,
      width = 11,
      height = 11
    )
  ),


  # forest: all plots --------------------------------------------------------


  tar_target(plot_forest, {
    mod <-
      if (m_key$target == "m_o_tt") {
        m_o_tt %>% pluck(m_key$index)
      } else if (m_key$target == "m_con_pain_sub") {
        m_con_pain_sub %>%
          pluck(m_key$index)
      } else if (m_key$target == "m_con_mood") {
        m_con_mood %>%
          pluck(m_key$index)
      } else if (m_key$target == "m_con_adverse") {
        m_con_adverse %>%
          pluck(m_key$index)
      }



    hpp_forest(mod$result,
               m_key)


  },
  pattern = map(m_key)),

  tar_target(plot_forest_write,
             {
               glue(
                 " Writing forest plot for {m_key$target}:
                        outcome {m_key$outcome}
                        timepoint {m_key$timepoint}
                        type {m_key$type}
                        condition {m_key$condition}"
               ) %>% msg_mine()

               msg_mine(m_key$forestpath)
               ggsave(
                 here::here("bksite",
                            m_key$forestpath),
                 plot_forest,
                 width = 11,
                 height = 11
               )
             },
             pattern = map(m_key, plot_forest)),



  # # nma summary -------------------------------------------------------------
  #
  # tar_target(m_nma_summary_fn,
  #            function(m) {
  #              m_stan_df <-
  #                m %>%
  #                summary() %>%
  #                as.data.frame()
  #
  #              m_stan_df %>%
  #                filter(str_detect(parameter, "^d\\[")) %>%
  #                arrange(mean) %>% # get lowest mean
  #                select(parameter, ci_lb = "2.5%",
  #                       ci_ub =  "97.5%",
  #                       mean, sd) %>%
  #                head(1) %>%
  #                mutate(
  #                  tau = m_stan_df %>% filter(parameter == "tau") %>% pluck("mean"),
  #                  outcome = m$network$agd_arm$outcome %>% unique(),
  #                  intervention = str_remove(parameter, "^d\\[") %>%
  #                    str_remove("\\]")
  #                ) %>%
  #                select(-parameter)
  #            }),
  #
  # tar_target(
  #   m_nma_summary_all,
  #   m_all_in %>%
  #     discard( ~ length(.x) == 1) %>%
  #     map_df(
  #       .f = function(m) {
  #         m_nma_summary_fn(m) %>%
  #           mutate(condition = "all")
  #       }
  #     )
  # ),
  #
  # tar_target(
  #   m_nma_summary_condition,
  #   m_condition %>%
  #     discard( ~ length(.x) == 1) %>%
  #     map_df(
  #       .f = function(m) {
  #         m_nma_summary_fn(m) %>%
  #           mutate(condition = m$network$agd_arm$condition_general
  #                  %>% unique())
  #       }
  #     )
  # ),
  #
  # tar_target(
  #   m_nma_summary,
  #   m_nma_summary_all %>%
  #     bind_rows(m_nma_summary_condition) %>%
  #     mutate(ci_contains_0 = ci_lb < 0 & ci_ub > 0) %>%
  #     select(outcome, condition, intervention, ci_contains_0, everything())
  # ),
  #
  # null --------------------------------------------------------------------

  NULL
)
