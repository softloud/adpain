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
  library(broom)
  library(latex2exp)
  library(crayon)
  library(here)

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
safe_hpp_nma <- safely(hpp_nma, otherwise = "failed")
# safe_rma <- safely(rma, otherwise = "failed")

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
    read_csv("data/obs_dat-2021-10-05 16:10:34.csv") %>%
      clean_names() %>%
      mutate(across(where(is.character), tolower)) %>%
      mutate(gs_row = row_number() + 1) %>%
      select(gs_row, everything())
  ),

  tar_target(
    r_outcome_key,
    read_csv("data/outcome-2021-10-06 11:44:20.csv") %>%
      clean_names()

  ),

  tar_target(
    r_scale_key,
    read_csv("data/scales-2021-10-05 11:50:08.csv") %>%
      clean_names() %>%
      rename(scale_dir = direction_of_improvement) %>%
      mutate(across(contains("scale"), tolower))
  ),

  # wrangle outcome key -----------------------------------------------------

  tar_target(
    w_outcome_key,
    r_outcome_key %>%
      mutate(
        outcome_nma = fct_relevel(
          outcome_nma,
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
        outcome_label = fct_reorder(outcome_label, outcome_nma, first)
      )
  ),

  # join observations to outcome
  tar_target(
    w_obs_outcome,
    r_obs_dat %>%
      left_join(w_outcome_key,
                by = "outcome")
  ),


  # scales ------------------------------------------------------------------
  tar_target(
    w_obs_scales,
    w_obs_outcome %>%
      left_join(r_scale_key, by = "scale")
  ),

  # wrangle obs dat ---------------------------------------------------------


  # calculations ------------------------------------------------------------


  tar_target(
    w_obs_calc,
    w_obs_scales %>%
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
                     se),

        # change direction
        mean = if_else(
          model_type == "smd" &
            direction_of_improvement == "higher" &
            scale_dir == "lower" & !is.na(scale_dir),
          -mean,
          mean
        ),
        mean = if_else(
          model_type == "smd" &
            direction_of_improvement == "lower" &
            scale_dir == "higher" & !is.na(scale_dir),
          -mean,
          mean
        )

      )
  ),


  # type of comparison ------------------------------------------------------



  tar_target(
    w_obs_type,
    w_obs_calc %>%
      mutate(
        intervention_category =
          case_when(
            intervention_grouping == "antidepressant" ~ "ad",
            intervention_grouping == "non-antidepressant pharmacological" ~ "nad_pharm",
            intervention_grouping == "non-pharmacological" ~ "npharm",
            intervention_grouping == "placebo" ~ intervention_grouping
          )
      )
  ),

  # all data wrangled -------------------------------------------------------

  tar_target(
    obs_excluded,
    w_obs_type %>%
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
          is.na(outcome) ~ "outcome not specified",
          model_type == "smd" &
            is.na(scale_dir) ~ "obs scale unmatched in scale key"
        )
      ) %>%
      filter(!is.na(exclusion_reason)) %>%
      select(exclusion_reason, everything())
  ),




  tar_target(
    obs_dat,
    w_obs_type %>%
      anti_join(obs_excluded) %>%
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


  # model dat ---------------------------------------------------------------
  tar_target(
    m_obs_dat,
    obs_dat %>%
      filter(
        outcome != "adverse_dropout",
        outcome != "adverse_number",
        outcome != "serious_adverse"
      ) %>%
      rename(
        study = study_id,
        type = intervention_category,
        condition = general_pain_grouping,
        class = ad_class,
        dose = ad_dose_categorised
      ) %>%
      ungroup() %>%
      #   # filter to two outcomes for testing
      filter(
        outcome %in% c(
          "sleep",
          "pain_int",
          "adverse",
          "mood",
          "pain_sub",
          "physical",
          "qol",
          "withdrawal",
          "pain_mod",
          NULL
        )
      )

  ),

  tar_target(
    m_type_label_key,
    m_obs_dat %>%
      select(intervention_grouping, type) %>%
      distinct() %>%
      rename(type_label = intervention_grouping)
  ),

  tar_target(
    m_timepoint_label_key,
    m_obs_dat %>%
      select(timepoint) %>%
      distinct() %>%
      mutate(
        timepoint_label =
          case_when(
            timepoint == "baseline" ~ "Baseline",
            timepoint == "post_int" ~ "Post-intervention",
            timepoint == "change_score" ~ "Change score",
            timepoint == "follow_up" ~ "Follow-up"
          )
      )

  ),

  # subgroups ---------------------------------------------------------------

  tar_target(
    subgroup_dat,
    m_obs_dat %>%
      filter(type != "placebo",
             timepoint != "baseline")
  ),

  tar_target(subgroup_fn,
             function(dat, subgroup) {
               dat %>%
                 summarise(
                   n_studies = n_distinct(study),
                   participants_int = sum(n),
                   studies = unique(study) %>% paste(collapse = ";"),
                   condition_s = unique(condition) %>% paste(collapse = ";"),
                   class_s = unique(class) %>% paste(collapse = ";"),
                   dose_s = unique(dose) %>% paste(collapse = ";"),
                   intervention_s = unique(intervention) %>%
                     paste(collapse = ";"),
                   .groups = "keep"
                 ) %>%
                 distinct() %>%
                 arrange(desc(studies)) %>%
                 filter(n_studies > 1) %>%
                 mutate(subgroup = subgroup)

             }),

  tar_target(
    subgroup_type,
    subgroup_dat %>%
      group_by(outcome, type, timepoint) %>%
      subgroup_fn("subgroup_type") %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_con,
    subgroup_dat %>%
      group_by(outcome, type, timepoint, condition) %>%
      subgroup_fn("subgroup_con") %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_con_class,
    subgroup_dat %>%
      group_by(outcome, type, timepoint, condition, class) %>%
      subgroup_fn("subgroup_con_class") %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_con_dose,
    subgroup_dat %>%
      group_by(outcome, type, timepoint, condition, dose) %>%
      subgroup_fn("subgroup_con_dose") %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    subgroup_con_class_dose,
    subgroup_dat %>%
      group_by(outcome, type, timepoint, condition, class, dose) %>%
      subgroup_fn("subgroup_con_class_dose") %>%
      tar_group(),
    iteration = "group"
  ),


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
                   forestpath = glue("images/forest/{filename}.png")
                 ) %>%
                 left_join(r_outcome_key, by = c("outcome", "model_type")) %>%
                 select(outcome, everything(), contains("path"))

             }),

  tar_target(m_dat_fn,
             function(s_dat) {
               subgroup_dat <-
                 s_dat %>%
                 mutate(study = str_split(studies, ";")) %>%
                 unnest(study)

               int_dat <-
                 subgroup_dat %>%
                 inner_join(m_obs_dat)

               placebo_dat <-
                 subgroup_dat %>%
                 # should remove class & dose
                 mutate(type = "placebo") %>%
                 inner_join(m_obs_dat)

               bind_rows(int_dat, placebo_dat) %>%
                 arrange(study) %>%
                 select(study, arm, everything()) %>%
                 viable_observations()

             }),

  # outcome, timepoint, type ------------------------------------------------


  tar_target(
    m_type_dat,
    {
      m_dat_fn(subgroup_type)
    },
    pattern = map(subgroup_type),
    iteration = "list"
  ),

  tar_target(check_viable,
             m_type_dat %>%
               pluck(1) %>%
               viable_observations()),

  tar_target(m_type_test,
             m_type_dat %>%
               pluck(1) %>%
               hpp_nma()),

  tar_target(
    m_type_check,
    subgroup_type %>%
      mutate(n_obs = nrow(m_type_dat)),
    pattern = map(subgroup_type, m_type_dat)
  ),

  tar_target(m_type_viable,
             m_type_check %>%
               filter(n_obs > 1)),

  tar_target(
    m_type,
    {
      this_dat <- m_type_viable %>% pull(tar_group)
      safe_hpp_nma(m_type_dat[[this_dat]])
    },
    pattern = map(m_type_viable),
    iteration = "list"
  ),


  tar_target(m_type_key, {
    m_key_fn(m_type, "m_type") %>%
      full_join(m_type_viable)
  },
  pattern = map(m_type, m_type_viable)),


  # condition ---------------------------------------------------------------
  tar_target(
    m_con_dat,
    {
      m_dat_fn(subgroup_con)
    },
    pattern = map(subgroup_con),
    iteration = "list"
  ),

  tar_target(
    m_con_check,
    subgroup_con %>%
      mutate(n_obs = nrow(m_con_dat)),
    pattern = map(subgroup_con, m_con_dat)
  ),

  tar_target(m_con_viable,
             m_con_check %>%
               filter(n_obs > 1)),

  tar_target(
    m_con,
    {
      this_dat <- m_con_viable %>% pull(tar_group)
      safe_hpp_nma(m_con_dat[[this_dat]])
    },
    pattern = map(m_con_viable),
    iteration = "list"
  ),


  tar_target(m_con_key, {
    m_key_fn(m_con, "m_con") %>%
      full_join(m_con_viable)
  },
  pattern = map(m_con, m_con_viable)),

  # wrangle model keys ------------------------------------------------------

  tar_target(
    m_key,
    bind_rows(m_type_key, m_con_key) %>%
      # list(m_type_key) %>%
      #   map_df(bind_rows) %>%
      group_by(target) %>%
      mutate(index = 1:n()) %>%
      select(target, index, everything()) %>%
      ungroup() %>%
      mutate(plot_index = row_number()) %>%
      left_join(m_timepoint_label_key) %>%
      left_join(m_type_label_key)

  ),

  # pairwise ----------------------------------------------------------------

  tar_target(pw_combn_fn,
             function(dat) {
               dat %>%
                 pull("intervention_s") %>%
                 str_split(pattern = ";") %>%
                 pluck(1) %>%
                 unique() %>%
                 c("placebo", .) %>%
                 combn(2) %>% {
                   tibble(int_1 = .[1,],
                          int_2 = .[2,])
                 } %>%
                 bind_cols(dat) %>%
                 mutate(comp = glue("{int_1}_{int_2}")) %>%
                 rename(comp_type = type)

             }),
  # get combinations
  tar_target(
    pw_type_combn,
    subgroup_type %>%
      pw_combn_fn()
    ,
    pattern = map(subgroup_type)
  ),

  tar_target(pw_study_fn,
             function(dat) {
               dat %>%
                 ungroup() %>%
                 inner_join(m_obs_dat) %>%
                 group_by(outcome, study, subgroup, timepoint) %>%
                 summarise(
                   interventions =
                     unique(intervention) %>% paste(collapse = ";"),
                   .groups = "keep"
                 ) %>%
                 mutate(int_split = str_split(interventions, ";"),
                        n_int = map_int(int_split, length)) %>%
                 left_join(dat)

             }),

  # identify what interventions studies have
  tar_target(
    pw_type_study,
    subgroup_type %>%
      bind_rows(subgroup_type %>%
                  mutate(type = "placebo")) %>%
    pw_study_fn(),
    pattern = map(subgroup_type)
  ),


  tar_target(pw_match_fn,
             function(combn, study_dat) {
               combn %>%
                 select(-subgroup) %>%
                 select(-tar_group) %>%
                 full_join(study_dat %>% select(-tar_group)) %>%
                 mutate(match = pmap_lgl(list(int_1, int_2, int_split),
                                         function(i1, i2, i) {
                                           i1 %in% i & i2 %in% i
                                         })) %>%
                 select(int_1, int_2, int_split, match, everything()) %>%
                 filter(match)

             }),

  # find studies that have both interventions
  tar_target(pw_type_match,
             pw_type_combn %>%
               pw_match_fn(pw_type_study)),

  tar_target(pw_group_fn,
             function(match_dat) {
               match_dat %>%
                 summarise(
                   n_studies = n_distinct(study),
                   studies = unique(study) %>% paste(collapse = ";"),
                   .groups = "keep"
                 ) %>%
                 filter(n_studies > 1) %>%
                 arrange(desc(n_studies)) %>%
                 mutate(study = str_split(studies, ";")) %>%
                 unnest(study)
             }),

  tar_target(
    pw_type_group,
    pw_type_match %>%
      group_by(outcome, comp_type, timepoint, comp, int_1, int_2) %>%
      pw_group_fn() %>%
      select(outcome, comp_type, comp, study, everything()) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(pw_wide_fn,
             function(group_dat) {
               int_dat <-
                 group_dat %>%
                 mutate(intervention = str_split(comp, "_")) %>%
                 unnest(intervention)

               set_class_dose <-
                 if ("dose" %in% names(int_dat) & "class" %in% names(int_dat)) {
                   int_dat %>%
                     mutate(
                       dose = if_else(intervention == "placebo", "n/a",
                                      dose),
                       class = if_else(intervention == "placebo", "n/a",
                                       class)
                     )

                 } else if ("dose" %in% names(int_dat)) {
                   int_dat %>%
                     mutate(
                       dose = if_else(intervention == "placebo", "n/a",
                                       dose)
                     )
                 } else if ("class" %in% names(int_dat)) {
                   int_dat %>%
                     mutate(
                       class = if_else(intervention == "placebo", "n/a",
                                       class)
                     )

                 } else {int_dat}

               # join to observations

               obs_dat <-
                 set_class_dose %>%
                 inner_join(m_obs_dat) %>%
                 group_split(intervention)


               obs_dat %>%
                 pluck(2) %>%
                 select(model_type,
                        outcome,
                        comp_type,
                        study,
                        intervention,
                        arm,
                        mean,
                        sd,
                        n,
                        r,
                        timepoint) %>%
                 rename_with( ~ glue("{.x}_comp"),
                              any_of(c(
                                "arm", "mean", "sd", "n", "r", "intervention"
                              ))) %>%
                 full_join(obs_dat[[1]])

             }),

  tar_target(
    pw_type_wide,
    pw_wide_fn(pw_type_group),
    pattern = map(pw_type_group),
    iteration = "list"
  ),

  tar_target(pw_type_ma_test,
             pw_type_wide %>%
               pluck(43) %>%
               hpp_rma()),

  tar_target(
    pw_type_ma,
    {
      print(pw_type_wide)

      pw_type_wide %>%
        hpp_rma()
    },
    pattern = map(pw_type_wide),
    iteration = "list"

  ),


  # pw condition ------------------------------------------------------------

  tar_target(
    pw_con_combn,
    subgroup_con %>%
      pw_combn_fn(),
    pattern = map(subgroup_con)
  ),

  # identify what interventions studies have
  tar_target(
    pw_con_study,
    subgroup_con %>%
      bind_rows(subgroup_con %>%
                  mutate(type = "placebo")) %>%
      pw_study_fn(),
    pattern = map(subgroup_con)
  ),

  tar_target(pw_con_match,
             pw_con_combn %>%
               pw_match_fn(pw_con_study)),

  tar_target(
    pw_con_group,
    pw_con_match %>%
      group_by(outcome, comp_type, timepoint, comp, int_1, int_2, condition) %>%
      pw_group_fn() %>%
      group_by(outcome, comp_type, comp, timepoint, condition) %>%
      select(outcome, comp_type, comp, study, everything()) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    pw_con_wide_test,
    pw_con_group %>%
      filter(tar_group == 11) %>%
      pw_wide_fn()
  ),

  tar_target(
    pw_con_wide,
    pw_con_group %>%
      pw_wide_fn(),
    pattern = map(pw_con_group),
    iteration = "list"
  ),

  tar_target(
    pw_con_ma,
    pw_con_wide %>%
      hpp_rma(),
    pattern = map(pw_con_wide),
    iteration = "list"

  ),


  # pw class ----------------------------------------------------------------
  tar_target(
    pw_con_class_combn,
    subgroup_con_class %>%
      pw_combn_fn(),
    pattern = map(subgroup_con_class)
  ),

  # identify what interventions studies have
  tar_target(
    pw_con_class_study,
    subgroup_con_class %>%
      bind_rows(subgroup_con_class %>%
                  mutate(type = "placebo",
                         class = "n/a"
                         )) %>%
      pw_study_fn(),
    pattern = map(subgroup_con_class)
  ),

  tar_target(
    pw_con_class_match,
    pw_con_class_combn %>%
      pw_match_fn(pw_con_class_study)
  ),

  tar_target(
    pw_con_class_group,
    pw_con_class_match %>%
      group_by(
        outcome,
        comp_type,
        timepoint,
        comp,
        int_1,
        int_2,
        condition,
        class
      ) %>%
      pw_group_fn() %>%
      group_by(outcome, comp_type, comp, timepoint, condition, class) %>%
      select(outcome, comp_type, comp, study, everything()) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    pw_con_class_wide_test,
    pw_con_class_group %>%
      filter(tar_group == 11) %>%
      pw_wide_fn()
  ),

  tar_target(
    pw_con_class_wide,
    pw_con_class_group %>%
      pw_wide_fn(),
    pattern = map(pw_con_class_group),
    iteration = "list"
  ),

  tar_target(
    pw_con_class_ma,
    pw_con_class_wide %>%
      hpp_rma(),
    pattern = map(pw_con_class_wide),
    iteration = "list"

  ),



# con dose ---------------------------------------------------------------

  tar_target(
    pw_con_dose_combn,
    subgroup_con_dose %>%
      pw_combn_fn(),
    pattern = map(subgroup_con_dose)
  ),

  # identify what interventions studies have
  tar_target(
    pw_con_dose_study,
    subgroup_con_dose %>%
      bind_rows(subgroup_con_dose %>%
                  mutate(type = "placebo",
                         dose = "n/a"
                  )) %>%
      pw_study_fn(),
    pattern = map(subgroup_con_dose)
  ),

  tar_target(
    pw_con_dose_match,
    pw_con_dose_combn %>%
      pw_match_fn(pw_con_dose_study)
  ),

  tar_target(
    pw_con_dose_group,
    pw_con_dose_match %>%
      group_by(
        outcome,
        comp_type,
        timepoint,
        comp,
        int_1,
        int_2,
        condition,
        dose
      ) %>%
      pw_group_fn() %>%
      select(outcome, comp_type, comp, study, everything()) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    pw_con_dose_wide_test,
    pw_con_dose_group %>%
      filter(tar_group == 11) %>%
      pw_wide_fn()
  ),

  tar_target(
    pw_con_dose_wide,
    pw_con_dose_group %>%
      pw_wide_fn(),
    pattern = map(pw_con_dose_group),
    iteration = "list"
  ),

  tar_target(
    pw_con_dose_ma,
    pw_con_dose_wide %>%
      hpp_rma(),
    pattern = map(pw_con_dose_wide),
    iteration = "list"

  ),


# class and dose ----------------------------------------------------------


tar_target(
  pw_con_class_dose_combn,
  subgroup_con_class_dose %>%
    pw_combn_fn(),
  pattern = map(subgroup_con_class_dose)
),

# identify what interventions studies have
tar_target(
  pw_con_class_dose_study,
  subgroup_con_class_dose %>%
    bind_rows(subgroup_con_class_dose %>%
                mutate(type = "placebo",
                       class = "n/a",
                       dose = "n/a"
                )) %>%
    pw_study_fn(),
  pattern = map(subgroup_con_class_dose)
),

tar_target(
  pw_con_class_dose_match,
  pw_con_class_dose_combn %>%
    pw_match_fn(pw_con_class_dose_study)
),

tar_target(
  pw_con_class_dose_group,
  pw_con_class_dose_match %>%
    group_by(
      outcome,
      comp_type,
      timepoint,
      comp,
      int_1,
      int_2,
      condition,
      class,
      dose
    ) %>%
    pw_group_fn() %>%
    select(outcome, comp_type, comp, study, everything()) %>%
    tar_group(),
  iteration = "group"
),

tar_target(
  pw_con_class_dose_wide_test,
  pw_con_class_dose_group %>%
    filter(tar_group == 11) %>%
    pw_wide_fn()
),

tar_target(
  pw_con_class_dose_wide,
  pw_con_class_dose_group %>%
    pw_wide_fn(),
  pattern = map(pw_con_class_dose_group),
  iteration = "list"
),

tar_target(
  pw_con_class_dose_ma,
  pw_con_class_dose_wide %>%
    hpp_rma(),
  pattern = map(pw_con_class_dose_wide),
  iteration = "list"
),


  # pw summary --------------------------------------------------------------

  tar_target(pw_results_fn,
             function(group_dat, ma_dat, subgroup_dat) {
               group_dat %>%
                 select(-study) %>%
                 ungroup() %>%
                 distinct() %>%
                 bind_cols(ma_dat  %>%  tidy(),
                           ma_dat  %>% glance()) %>%
                 clean_names() %>%
                 mutate(mod = list(ma_dat),
                        rma_class = class(ma_dat) %>% pluck(1)
                        ) %>%
                 relocate(starts_with('mod'), .before = outcome) %>%
                 left_join(w_outcome_key, by = c("outcome" = "outcome_nma")) %>%
                 select(-type) %>%
                 rename(type = comp_type) %>%
                 left_join(m_type_label_key) %>%
                 left_join(m_timepoint_label_key) %>%
                 left_join(subgroup_dat %>%
                             select(-studies,-n_studies,-tar_group))


             }),

  tar_target(pw_type_results, {
    pw_results_fn(pw_type_group, pw_type_ma, subgroup_type)
  },
  pattern = map(pw_type_ma, pw_type_group)),

  tar_target(pw_con_results, {
    pw_results_fn(pw_con_group, pw_con_ma, subgroup_con)
  },
  pattern = map(pw_con_ma, pw_con_group)),

  tar_target(pw_con_class_results, {
    pw_results_fn(pw_con_class_group, pw_con_class_ma, subgroup_con_class)
  },
  pattern = map(pw_con_class_ma, pw_con_class_group)),

tar_target(pw_con_dose_results, {
  pw_results_fn(pw_con_dose_group, pw_con_dose_ma, subgroup_con_dose)
},
pattern = map(pw_con_dose_ma, pw_con_dose_group)),

tar_target(pw_con_class_dose_results, {
  pw_results_fn(pw_con_class_dose_group, pw_con_class_dose_ma, subgroup_con_class_dose)
},
pattern = map(pw_con_class_dose_ma, pw_con_class_dose_group)),

  tar_target(
    pw_results,
    bind_rows(pw_type_results,
              pw_con_results,
              pw_con_class_results,
              pw_con_class_dose_results
              ) %>%
      mutate(
        condition = if_else(
          is.na(condition),
          "all conditions",
          condition
        ) %>% fct_relevel("all conditions"),
        class = if_else(
          is.na(class),
          "all classes",
          class
        ) %>% fct_relevel("all classes"),
        dose = if_else(
          is.na(dose),
          "all doses",
          dose
        ) %>% fct_relevel("all doses"),
        timepoint = fct_relevel(timepoint, "post_int")
      ) %>%
      arrange(outcome, condition, type, class, dose) %>%
      mutate(
        pw_forest_file =
          glue(
            "images/pw-forest/{subgroup}-{outcome}-{condition}-{type}-{timepoint}-{class}-{dose}-{comp}.png"
          )
      )
  ),


  # pw forest ---------------------------------------------------------------

  tar_target(pw_forest_dev, {
    this_mod <- pw_results %>%
      # filter(
      #   comp_type == "ad",
      #   outcome == "pain_sub",
      #   timepoint == "post_int",
      #   comp == "placebo_desvenlafaxine"
      # )
      # filter(subgroup == "subgroup_con") %>%
      sample_n(1)

    # this_mod
    pw_forest(this_mod)
  }),

  tar_target(
    pw_plot,
    pw_results %>%
      filter(rma_class == "rma.mv") %>%
      mutate(
        plot_height = n_studies / max(n_studies),
        plot_height = if_else(plot_height < 0.5, 0.7, plot_height)
      ) %>%
      select(-tar_group) %>%
      ungroup()
  ),

  tar_target(pw_forest_write, {
    pw_forest(pw_plot)

    here("bksite", pw_plot$pw_forest_file) %>%
      # here(pw_results$pw_forest_file) %>%
      ggsave(width = 7, height = 8 * pw_plot$plot_height)
  },
  pattern = map(pw_plot)),

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
      m_key$target == "m_type" ~
        glue(
          "Subgroups: timepoint [{m_key$timepoint_label}] and type [{m_key$type_label}]"
        ),
      str_detect(m_key$target, "^m_con") ~
        glue(
          "Subgroups: timepoint [{m_key$timepoint_label}], type [{m_key$type_label}], condition [{m_key$condition_s}]"
        )
    )

    this_subtitle %>% print()

    mod <-
      if (m_key$target == "m_type") {
        m_type[[m_key$index]]
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
                 " Writing net plot for m_type:
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
      m_type %>%
      pluck(1, "result")

    key <- m_key %>%
      filter(target == "m_type", index == 1)

    # print(summary(this_mod))

    msg_mine("Identify the dataframe req for conf ints text")
    # this_mod

    msg_mine("Plot generic forest")

    forest_multinma(this_mod, key)
  }),


  # forest: dev -------------------------------------------------------------
  tar_target(plot_forest_dev, {
    # select an arbitrary lor model
    m_type_key_row <-
      m_key %>%
      filter(outcome == "pain_sub",
             target == "m_type") %>%
      head(1)

    msg_mine(" Selected row from m_model_key")
    print(m_type_key_row)

    msg_mine(" Get model")
    mod <-
      m_type %>%
      pluck(m_type_key_row$index) %>%
      pluck("result")

    msg_mine(" Create plot")
    hpp_forest(mod,
               m_type_key_row)

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
      if (m_key$target == "m_type") {
        m_type %>% pluck(m_key$index)
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




  # analysis table ----------------------------------------------------------

  tar_target(
    m_analysis,
    bind_rows(
      subgroup_type,
      subgroup_con,
      subgroup_con_class,
      subgroup_con_class_dose
    ) %>%
      mutate(
        subgroup = fct_relevel(
          subgroup,
          "subgroup_type",
          "subgroup_con",
          "subgroup_con_class",
          "subgroup_con_class_dose"
        )
      ) %>%
      arrange(outcome, subgroup, type, class, dose)
  ),
  # null --------------------------------------------------------------------

  NULL
)
