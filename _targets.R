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
  library(adpain)
  library(ggpubr)
  library(patchwork)
  library(kableExtra)
  library(scales)

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
safe_regtest <- safely(regtest, otherwise = "failed")
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
    read_csv("data-raw/obs_dat-2021-12-08 02:20:22.csv") %>%
      clean_names() %>%
      mutate(across(where(is.character), tolower)) %>%
      mutate(gs_row = row_number() + 1) %>%
      select(gs_row, everything())
  ),

  tar_target(
    r_outcome_key,
    read_csv("data-raw/outcome-2021-11-23 17:34:26.csv") %>%
      clean_names()

  ),

  tar_target(
    r_scale_key,
    read_csv("data-raw/scales-2021-11-23 17:32:14.csv") %>%
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
      ) %>%
      arrange(outcome_nma)
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
        n_calc = is.na(n) & r > 0 & r_percent > 0,
        n = if_else(n_calc,
                    round(r / r_percent),
                    n) %>% as.integer(),

        # get r from n and r percent
        r_calc = is.na(r) & r_percent > 0 & n > 0,
        r = if_else(r_calc,
                    round(r_percent * n),
                    r) %>% as.integer(),

        # estimate mean & se from median and iqr
        mean_calc = is.na(mean) &
          !is.na(median) &
          !is.na(iqr_lower) &
          !is.na(iqr_higher),
        mean = if_else(
          mean_calc,
          varameta::wan_mean_C3(q_1 = iqr_lower,
                                m = median,
                                q_3 = iqr_higher),
          mean
        ),
        se_med_calc =           is.na(se) & is.na(sd) &
          !is.na(median) &
          !is.na(iqr_lower) & !is.na(iqr_higher) & !is.na(n),
        se = if_else(
          se_med_calc,
          varameta::wan_se_C3(
            q_1 = iqr_lower,
            m = median,
            q_3 = iqr_higher,
            n = n
          ),
          se
        ),

        # calculate sd & se
        sd_med_upper_calc =           is.na(se) &
          is.na(sd) & !is.na(mean) & !is.na(n) & !is.na(ci_upper),
        sd = if_else(
          sd_med_upper_calc,
          sqrt(n) * (ci_upper - mean) / qnorm(1 - 0.05 / 2) ,
          sd
        ),
        sd_calc_med_lower =           is.na(se) &
          is.na(sd) & !is.na(mean) & !is.na(n) & !is.na(ci_lower),
        sd = if_else(
          sd_calc_med_lower,
          sqrt(n) * (mean - ci_lower) / qnorm(1 - 0.05 / 2),
          sd
        ),

        sd_na_calc = se > 0 & n > 0 & is.na(sd),
        sd = if_else(sd_na_calc,
                     se * sqrt(n),
                     sd),
        sd_na_calc = sd > 0 & n > 0 & is.na(se),
        se = if_else(sd_na_calc,
                     sd / sqrt(n),
                     se),

        # change direction
        mean_dir =           model_type == "smd" &
          direction_of_improvement == "higher" &
          scale_dir == "lower" & !is.na(scale_dir),
        mean = if_else(mean_dir, -mean,
                       mean),

        mean_calc = model_type == "smd" &
          direction_of_improvement == "lower" &
          scale_dir == "higher" & !is.na(scale_dir),
        mean = if_else(mean_calc, -mean,
                       mean)
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


  # calculate rob across subgroups ------------------------------------------

  tar_target(
    rob,
    mod_dat %>%
      group_by(outcome, type, timepoint, intervention) %>%
      count(rob) %>%
      pivot_wider(
        names_from = rob,
        values_from = n,
        values_fill = 0
      ) %>%
      mutate(high_risk_p = high / (high + low)) %>%
      select(-high,-low)
  ),

  tar_target(write_rob,
             usethis::use_data(rob, overwrite = TRUE)),


  # keys --------------------------------------------------------------------


  tar_target(
    outcome_key,
    w_outcome_key %>%
      filter(outcome != "adverse_number", !str_detect(outcome,  "mood_")) %>%
      # remove for pgic calcs tomorrow!str_detect(outcome, "pgic")) %>%
      select(-outcome) %>%
      rename(outcome = outcome_nma)
  ),

  tar_target(
    write_outcome_key,
    usethis::use_data(outcome_key, overwrite = TRUE)
  ),

  tar_target(outcomes,
             outcome_key %>%
               arrange(outcome) %>%
               pull(outcome)),

  tar_target(
    type_key,
    obs_dat %>%
      select(intervention_grouping, type = intervention_category) %>%
      distinct() %>%
      rename(type_label = intervention_grouping)
  ),

  tar_target(write_type_key,
             usethis::use_data(type_key, overwrite = TRUE)),

  tar_target(
    timepoint_key,
    obs_dat %>%
      select(timepoint) %>%
      distinct() %>%
      mutate(
        timepoint_label =
          case_when(
            timepoint == "baseline" ~ "baseline",
            timepoint == "post_int" ~ "post-intervention",
            timepoint == "change_score" ~ "change score",
            timepoint == "follow_up" ~ "follow-up"
          )
      )

  ),

  tar_target(
    write_timepoint_key,
    usethis::use_data(timepoint_key, overwrite = TRUE)
  ),





  # rob ---------------------------------------------------------------------

  tar_target(
    rob_calc,
    obs_dat %>%
      mutate(
        rob_factors = str_c(
          sequence_generation,
          allocation_concealment,
          blinding_of_participants_and_personnel,
          blinding_of_outcome_assessors,
          incomplete_outcome_data,
          selective_outcome_reporting,
          other_sources_of_bias,
          sep = "; "
        ),
        rob = if_else(str_detect(rob_factors, "high"),
                      "high",
                      "low"),
        rob_subcat = case_when(
          str_detect(rob_factors, "high") &
            str_detect(rob_factors, "unclear") ~ "high + unclear",
          str_detect(rob_factors, "high") ~ "high",
          str_detect(rob_factors, "unclear") ~ "unclear",
          TRUE ~ "low"
        )

      ) %>%
      select(starts_with("rob"), everything())
  ),

  # study_length ------------------------------------------------------------

  tar_target(
    study_length,
    rob_calc %>%
      select(outcome, study_id, study_label = length_of_study_start_to_post_intervention) %>%
      distinct() %>%
      mutate(
        study_label = str_replace(study_label, "9 months", "36 weeks"),
        weeks = str_extract(study_label, "\\d+") %>% as.integer()
      ) %>%
      rename(study = study_id)
  ),
  # mod dat -----------------------------------------------------------------

  tar_target(
    mod_dat,
    rob_calc %>%
      filter(outcome != "adverse_number") %>%
      rename(
        study = study_id,
        type = intervention_category,
        condition = general_pain_grouping,
        class = ad_class,
        dose = ad_dose_categorised
      ) %>%
      ungroup() %>%
      left_join(timepoint_key) %>%
      left_join(type_key) %>%
      left_join(outcome_key) %>%
      # deal with long interventions
      mutate(
        intervention =
          str_replace(intervention, "cognitive behaviou*ral therapy", "cbt") %>%
          str_replace("physical fitness training", "pft") %>%
          str_replace("coping skills training", "cst"),

        # relevel
        dose = fct_relevel(dose, "high", "standard", "low"),
        intervention = fct_relevel(intervention,
                                   "duloxetine",
                                   "amitriptyline",
                                   "milnacipran")
      ) %>%
      filter(!(
        round(mean) == 39 &
          study == "tetreault" &
          intervention == "placebo"
      )) %>%
      left_join(study_length %>% select(-study_label)) %>%
      mutate(duration = if_else(weeks > 12, "PI > 12", "PI < 12")) %>%
      filter(
        !(
          outcome == "pain_int" &
            timepoint == "post_int" &
            round(mean) == 39 &
            study == "tetreault 2018" & intervention == "placebo"
        )
      )

  ),


  # create mod dat object ---------------------------------------------------

  tar_target(write_mod_dat,
             usethis::use_data(mod_dat, overwrite = TRUE)),



  tar_target(
    intervention_key,
    mod_dat %>%
      select(intervention, type, class) %>%
      distinct() %>%
      left_join(type_key)
  ),


  # check nma ---------------------------------------------------------------


  # we will always want to have all-in nma
  tar_target(nma_dat_dev,
             get_nma_dat(mod_dat, outcome == "adverse")),

  tar_target(nma_net_dev,
             hpp_net(nma_dat_dev, type = "lor")),

  tar_target(nma_mod_dev,
             # calls hpp_net
             hpp_nma(nma_dat_dev)),

  tar_target(
    nma_forest_dev,
    hpp_forest(nma_mod_dev, mod_type = "lor", dir = "lower")
  ),

  tar_target(
    nma_for_class_dev,
    hpp_forest(
      nma_mod_dev,
      mod_type = "lor",
      dir = "lower",
      this_class = "snri"
    )
  ),


  # check pw ----------------------------------------------------------------

  tar_target(
    pw_dat_dev,
    get_pw_dat(
      dat = mod_dat,
      outcome = "pain_sub",
      timepoint = "post_int",
      g1 = "duloxetine",
      g2 = "placebo"
    )

  ),

  tar_target(pw_rma_dev,
             pw_dat_dev %>%
               hpp_rma()),

  tar_target(
    pw_for_dev,
    pw_forest_plot(
      pw_rma_dev$rma_mv,
      pw_dat_dev,
      m_type = "smd",
      dir = "lower"
    )
  ),

  tar_target(
    pw_for_caption,
    pw_forest_plot(
      pw_rma_dev$rma_mv,
      pw_dat_dev,
      m_type = "smd",
      dir = "lower"
    ) +
      labs(caption = "test")
  ),

  tar_target(
    pw_for_condition,
    pw_for_dev +
      facet_grid(condition ~ .,
                 scales = "free",
                 space = "free")
  ),

  tar_target(
    pw_for_dose,
    pw_for_dev +
      facet_grid(dose ~ .,
                 scales = "free",
                 space = "free")
  ),

  # outcome-level analysis --------------------------------------------------
  tar_target(o_groups, {
    mod_dat %>%
      filter(timepoint != "follow_up",
             timepoint != "baseline") %>%
      group_by(outcome,
               timepoint) %>%
      summarise(n_studies = n_distinct(study)) %>%
      distinct() %>%
      filter(timepoint != "followup") %>%
      mutate(
        dat = pmap(
          list(o = outcome,
               tp = timepoint),
          .f = function(o, tp) {
            mod_dat %>%
              filter(outcome == o,
                     timepoint == tp) %>%
              viable_observations()

          }

        ),
        participants = map_int(dat,
                               ~ sum(.x$n)),
        placebo_check = map_lgl(dat,
                                ~ any("placebo" == .x$type))
      ) %>%
      # this should be documented
      filter(placebo_check,
             participants > 200) %>%
      select(-placebo_check)
  }),

  tar_target(
    o_nma,
    o_groups$dat[[1]] %>%
      safe_hpp_nma(),
    pattern = map(o_groups),
    iteration = "list"
  ),

  tar_target(o_nma_mod,
             {
               error_status <- !is.null(o_nma$error)

               o_groups %>%
                 ungroup() %>%
                 mutate(error = error_status)
             }
             ,
             pattern = map(o_nma, o_groups)),

  tar_target(
    o_nma_key,
    o_nma_mod %>%
      mutate(model_index = row_number()) %>%
      filter(error == FALSE) %>%
      mutate(
        write_path = glue(
        "outputs/nma-otp/{outcome}-{timepoint}.rds"
      ))
  ),

  tar_target(write_o_nma, {
    this_mod <-
      o_nma %>%
      pluck(o_nma_key$model_index)

    write_rds(this_mod, o_nma_key$write_path)
  },
  pattern = map(o_nma_key)),


  tar_target(
    o_nma_rel,
    o_nma[[o_nma_key$model_index]] %>%
      pluck('result') %>%
      extract_nma_estimates() %>%
      filter(par == "d") %>%
      rename(intervention = par_index) %>%
      select(-par) %>%
      bind_cols(o_nma_key) %>%
      select(outcome, timepoint, intervention,
             mean, sd, ci_lb, ci_ub) %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      mutate(
        mean = if_else(model_type == "lor",
                       exp(mean), mean),
        ci_lb = if_else(model_type == "lor",
                        exp(ci_lb), ci_lb),
        ci_ub = if_else(model_type == "lor",
                        exp(ci_ub), ci_ub)

      ) %>%
      rename_with(~ glue("nma_{.x}"),
                  c(mean, sd, ci_lb, ci_ub))
    ,
    pattern = map(o_nma_key)
  ),

  tar_target(
    o_nma_rank,
    o_nma[[o_nma_key$model_index]] %>%
      pluck("result") %>%
      posterior_ranks(lower_better = (
        outcome_dir(o_nma_key$outcome) == "lower"
      )) %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(
        intervention = parameter %>%
          str_remove("rank\\[") %>%
          str_remove("\\]"),
        rank_ci_lb = x2_5_percent,
        rank_ci_ub = x97_5_percent,
        rank_mean = mean
      ) %>%
      bind_cols(o_nma_key) %>%
      select(outcome, timepoint,
             intervention, starts_with("rank")),
    pattern = map(o_nma_key)
  ),

  tar_target(
    o_nma_rank_rel,
    o_nma_key %>%
      left_join(o_nma_rel) %>%
      left_join(o_nma_rank) %>%
      rename(nma_index = model_index)
  ),

  tar_target(o_nma_results,
             o_nma_rank_rel),



  # outcome-level contrast --------------------------------------------------

  tar_target(contrast_nma_init,
             o_groups),

  tar_target(
    contrast_dat_dev,
    contrast_nma_init %>%
      mutate(model_type = map_chr(outcome, outcome_mod)) %>%
      filter(model_type == "smd") %>%
      ungroup() %>%
      filter(outcome == "mood",
             timepoint == "post_int"
             ) %>%
      sample_n(1) %>%
      pluck("dat", 1) %>%
      # select only what is req for dev
      select(outcome, timepoint, study, arm, intervention, mean, sd, se, n) %>%
      mutate(intervention = fct_relevel(intervention, "placebo")) %>%
      arrange(study, intervention) %>%
      group_by(study) %>%
      mutate(trt_ref_study = first(arm)) %>%
      ungroup() %>%
      distinct() %>%
      # remove after troubleshooting
      filter(study != "creed 2003")

  ),

  tar_target(
    contrast_dups_check,
    contrast_dat_dev %>%
      filter(arm == trt_ref_study) %>%
      count(study) %>%
      filter(n > 1)
  ),

  tar_target(
    contrast_control_dev,
    contrast_dat_dev %>%
      filter(arm == trt_ref_study) %>%
      rename_with(
        ~ glue("{.x}_control"),-c(study, arm, intervention, trt_ref_study)
      ) %>%
      distinct()

  ),

  tar_target(contrast_wide_dev,
             {
               contrast_dat_dev %>%
                 filter(arm != trt_ref_study) %>%
                 left_join(contrast_control_dev %>%
                             select(-arm,-intervention,-trt_ref_study)) %>%
                 ungroup() %>%
                 arrange(study)
             }),

  tar_target(
    contrast_escalc_dev,
    escalc(
      m1i = mean,
      sd1i = sd,
      n1i = n,
      m2i = mean_control,
      sd2i = sd_control,
      n2i = n_control,
      measure = "SMD",
      data = contrast_wide_dev
    )
  ),

  tar_target(
    contrast_netdat_dev,
    contrast_escalc_dev %>%
      select(study, yi, vi, n, intervention, arm) %>%
      bind_rows(
        contrast_control_dev %>%
          select(study, diff_se = se_control,
                 n = n_control,
                 intervention, arm)
      ) %>%
      arrange(study) %>%
      mutate(diff = yi,
             diff_se = if_else(is.na(diff_se),
                               sqrt(vi),
                               diff_se))
  ),

  tar_target(
    contrast_net_dev,
      set_agd_contrast(
        data = contrast_netdat_dev,
        study = study,
        trt = intervention,
        y = diff,
        se = diff_se,
        sample_size = n,
        trt_ref = "placebo"
      )
  ),

  tar_target(
    contrast_nma_dev,
    nma(contrast_net_dev, trt_effects = "random")
  ),


  # contrast smd calc -------------------------------------------------------


  tar_target(
    contrast_dat,
    mod_dat %>%
      filter(
        model_type == "smd",
        timepoint %in% c("change_score", "post_int")
      ) %>%
      # select only what is req for dev
      select(outcome, timepoint, study, arm, intervention, mean, sd, n) %>%
      mutate(intervention = fct_relevel(intervention, "placebo")) %>%
      arrange(study, intervention) %>%
      group_by(study) %>%
      mutate(trt_ref_study = first(arm)) %>%
      ungroup() %>%
      distinct()
  ),

  tar_target(
    contrast_dups,
    contrast_dat %>%
      filter(intervention == "placebo") %>%
      count(outcome, timepoint, study) %>%
      filter(n > 1)
  ),

  tar_target(contrast_wide,
             {
               control_dat <-
                 contrast_dat %>%
                 filter(arm == trt_ref_study) %>%
                 select(-arm,-intervention,-trt_ref_study) %>%
                 rename_with( ~ glue("{.x}_control"),
                              c(mean, sd, n))

               contrast_dat %>%
                 ungroup() %>%
                 filter(arm != trt_ref_study) %>%
                 left_join(control_dat,
                           by = c("outcome", "timepoint", "study")) %>%
                 arrange(outcome, timepoint, study)

             }),

  tar_target(
    contrast_groups,
    contrast_wide %>%
      group_by(outcome, timepoint) %>%
      arrange(outcome, timepoint) %>%
      tar_group(),
    iteration = "group"
  ),


  tar_target(
    contrast_escalc,
    escalc(
      m1i = mean,
      sd1i = sd,
      n1i = n,
      m2i = mean_control,
      sd2i = sd_control,
      n2i = n_control,
      slab = study,
      measure = "SMD",
      data = contrast_groups
    ),
    pattern = map(contrast_groups),
    iteration = "list"
  ),


  # conditions --------------------------------------------------------------
  tar_target(con_groups, {
    mod_dat %>%
      filter(timepoint != "follow_up",
             timepoint != "baseline") %>%
      group_by(outcome,
               timepoint,
               condition) %>%
      summarise(n_studies = n_distinct(study)) %>%
      distinct() %>%
      filter(timepoint != "followup") %>%
      mutate(
        # dat is for outcome, timepoint, condition
        dat = pmap(
          list(o = outcome,
               tp = timepoint,
               con = condition),
          .f = function(o, tp, con) {
            mod_dat %>%
              filter(outcome == o,
                     timepoint == tp,
                     condition == con) %>%
              viable_observations()

          }

        ),
        participants = map_int(dat,
                               ~ sum(.x$n)),
        placebcon_check = map_lgl(dat,
                                  ~ any("placebo" == .x$type))
      ) %>%
      # this should be documented
      filter(placebcon_check,
             participants > 200) %>%
      select(-placebcon_check)
  }),

  tar_target(
    con_output_structure,
    con_groups %>%
      mutate(intervention = map(
        dat, ~ as.character(.x$intervention) %>% unique()
      )) %>%
      unnest(intervention) %>%
      select(outcome,
             timepoint,
             condition,
             intervention,
             everything()) %>%
      filter(intervention != "placebo")
  ),

  tar_target(
    con_nma,
    con_groups$dat[[1]] %>%
      safe_hpp_nma(),
    pattern = map(con_groups),
    iteration = "list"
  ),

  tar_target(con_nma_mod,
             {
               error_status <- !is.null(con_nma$error)

               con_groups %>%
                 ungroup() %>%
                 mutate(error = error_status)
             }
             ,
             pattern = map(con_nma, con_groups)),

  tar_target(
    con_nma_key,
    con_nma_mod %>%
      mutate(model_index = row_number()) %>%
      filter(error == FALSE) %>%
      mutate(
        write_path = glue("outputs/nma-otp/{outcome}-{timepoint}-{condition}.rds")
      )
  ),

  tar_target(write_con_nma, {
    this_mod <-
      con_nma %>%
      pluck(con_nma_key$model_index)

    write_rds(this_mod, con_nma_key$write_path)
  },
  pattern = map(con_nma_key)),


  tar_target(
    con_nma_rel,
    con_nma[[con_nma_key$model_index]] %>%
      pluck('result') %>%
      extract_nma_estimates() %>%
      filter(par == "d") %>%
      rename(intervention = par_index) %>%
      select(-par) %>%
      bind_cols(con_nma_key) %>%
      # needed to add condition here
      select(
        outcome,
        timepoint,
        intervention,
        condition,
        mean,
        sd,
        ci_lb,
        ci_ub
      ) %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      mutate(
        mean = if_else(model_type == "lor",
                       exp(mean), mean),
        ci_lb = if_else(model_type == "lor",
                        exp(ci_lb), ci_lb),
        ci_ub = if_else(model_type == "lor",
                        exp(ci_ub), ci_ub)

      ) %>%
      rename_with(~ glue("nma_{.x}"),
                  c(mean, sd, ci_lb, ci_ub))
    ,
    pattern = map(con_nma_key)
  ),

  tar_target(
    con_nma_rank,
    con_nma[[con_nma_key$model_index]] %>%
      pluck("result") %>%
      posterior_ranks() %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(
        intervention = parameter %>%
          str_remove("rank\\[") %>%
          str_remove("\\]"),
        rank_ci_lb = x2_5_percent,
        rank_ci_ub = x97_5_percent,
        rank_mean = mean
      ) %>%
      bind_cols(con_nma_key) %>%
      # needed to add condition here
      select(
        outcome,
        timepoint,
        condition,
        intervention,
        starts_with("rank")
      ),
    pattern = map(con_nma_key)
  ),

  tar_target(
    con_nma_rank_rel,
    con_nma_key %>%
      select(-error) %>%
      rename(nma_studies = n_studies,
             nma_participants = participants) %>%
      left_join(con_nma_rel, by = c("outcome", "timepoint", "condition")) %>%
      # full_join(con_nma_rank) %>%
      rename(nma_index = model_index)
  ),

  tar_target(
    con_nma_results,
    con_nma_rank_rel %>%
      select(outcome, intervention, timepoint, condition,
             everything())
  ),
  # duration --------------------------------------------------------------
  tar_target(dur_group_setup, {
    mod_dat %>%
      # create tag for duration
      left_join(study_length %>% select(-study_label)) %>%
      filter(timepoint != "follow_up",
             timepoint != "baseline") %>%
      group_by(outcome,
               timepoint,
               duration) %>%
      summarise(n_studies = n_distinct(study)) %>%
      distinct() %>%
      mutate(# dat is for outcome, timepoint, duration
        dat = pmap(
          list(o = outcome,
               tp = timepoint,
               con = duration),
          .f = function(o, tp, con) {
            mod_dat %>%
              filter(outcome == o,
                     timepoint == tp,
                     duration == con) %>%
              viable_observations()

          }

        ))
  }),

  tar_target(
    dur_groups,
    dur_group_setup %>%
      filter(n_studies > 1) %>%
      mutate(
        participants = map_int(dat,
                               ~ sum(.x$n)),
        placebdur_check = map_lgl(dat,
                                  ~ any(.x$intervention == "placebo"))
      ) %>%
      filter(placebdur_check) %>%
      select(-placebdur_check)

  ),

  tar_target(
    dur_output_structure,
    dur_groups %>%
      mutate(intervention = map(
        dat, ~ as.character(.x$intervention) %>% unique()
      )) %>%
      unnest(intervention) %>%
      select(outcome,
             timepoint,
             duration,
             intervention,
             everything()) %>%
      filter(intervention != "placebo")
  ),

  tar_target(
    dur_nma,
    dur_groups$dat[[1]] %>%
      safe_hpp_nma(),
    pattern = map(dur_groups),
    iteration = "list"
  ),

  tar_target(dur_nma_mod,
             {
               error_status <- !is.null(dur_nma$error)

               dur_groups %>%
                 ungroup() %>%
                 mutate(error = error_status)
             }
             ,
             pattern = map(dur_nma, dur_groups)),

  tar_target(
    dur_nma_key,
    dur_nma_mod %>%
      mutate(model_index = row_number()) %>%
      filter(error == FALSE) %>%
      mutate(
        write_path = glue("outputs/nma-otp/{outcome}-{timepoint}-{duration}.rds")
      )
  ),

  tar_target(write_dur_nma, {
    this_mod <-
      dur_nma %>%
      pluck(dur_nma_key$model_index)

    # write_rds(this_mod, dur_nma_key$write_path)
  },
  pattern = map(dur_nma_key)),


  tar_target(
    dur_nma_rel,
    dur_nma[[dur_nma_key$model_index]] %>%
      pluck('result') %>%
      extract_nma_estimates() %>%
      filter(par == "d") %>%
      rename(intervention = par_index) %>%
      select(-par) %>%
      bind_cols(dur_nma_key) %>%
      # needed to add duration here
      select(
        outcome,
        timepoint,
        intervention,
        duration,
        mean,
        sd,
        ci_lb,
        ci_ub
      ) %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      mutate(
        mean = if_else(model_type == "lor",
                       exp(mean), mean),
        ci_lb = if_else(model_type == "lor",
                        exp(ci_lb), ci_lb),
        ci_ub = if_else(model_type == "lor",
                        exp(ci_ub), ci_ub)

      ) %>%
      rename_with(~ glue("nma_{.x}"),
                  c(mean, sd, ci_lb, ci_ub))
    ,
    pattern = map(dur_nma_key)
  ),

  tar_target(
    dur_nma_rank,
    dur_nma[[dur_nma_key$model_index]] %>%
      pluck("result") %>%
      posterior_ranks() %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(
        intervention = parameter %>%
          str_remove("rank\\[") %>%
          str_remove("\\]"),
        rank_ci_lb = x2_5_percent,
        rank_ci_ub = x97_5_percent,
        rank_mean = mean
      ) %>%
      bind_cols(dur_nma_key) %>%
      # needed to add duration here
      select(
        outcome,
        timepoint,
        duration,
        intervention,
        starts_with("rank")
      ),
    pattern = map(dur_nma_key)
  ),

  tar_target(
    dur_nma_rank_rel,
    dur_nma_key %>%
      select(-error) %>%
      rename(nma_studies = n_studies,
             nma_participants = participants) %>%
      left_join(dur_nma_rel, by = c("outcome", "timepoint", "duration")) %>%
      # full_join(dur_nma_rank) %>%
      rename(nma_index = model_index)
  ),

  tar_target(
    dur_nma_results,
    dur_nma_rank_rel %>%
      select(outcome, intervention, timepoint, duration,
             everything())
  ),

  # dose --------------------------------------------------------------
  tar_target(dose_groups, {
    mod_dat %>%
      filter(timepoint != "follow_up",
             timepoint != "baseline") %>%
      group_by(outcome,
               timepoint,
               dose) %>%
      summarise(n_studies = n_distinct(study)) %>%
      distinct() %>%
      mutate(
        # dat is for outcome, timepoint, dose
        dat = pmap(
          list(o = outcome,
               tp = timepoint,
               subgroup = dose),
          .f = function(o, tp, subgroup) {
            mod_dat %>%
              filter(outcome == o,
                     timepoint == tp,
                     dose %in% c(subgroup, "n/a")) %>%
              viable_observations()

          }

        ),
        participants = map_int(dat,
                               ~ sum(.x$n)),
        placebdose_check = map_lgl(dat,
                                   ~ any("placebo" == .x$type))
      ) %>%
      # this should be documented
      filter(placebdose_check) %>%
      # ,
      # participants > 200) %>%
      select(-placebdose_check)
  }),

  tar_target(
    dose_output_structure,
    dose_groups %>%
      mutate(intervention = map(
        dat, ~ as.character(.x$intervention) %>% unique()
      )) %>%
      unnest(intervention) %>%
      select(outcome,
             timepoint,
             dose,
             intervention,
             everything()) %>%
      filter(intervention != "placebo")
  ),

  tar_target(
    dose_nma,
    dose_groups$dat[[1]] %>%
      safe_hpp_nma(),
    pattern = map(dose_groups),
    iteration = "list"
  ),

  tar_target(dose_nma_mod,
             {
               error_status <- !is.null(dose_nma$error)

               dose_groups %>%
                 ungroup() %>%
                 mutate(error = error_status)
             }
             ,
             pattern = map(dose_nma, dose_groups)),

  tar_target(
    dose_nma_key,
    dose_nma_mod %>%
      mutate(model_index = row_number()) %>%
      filter(error == FALSE) %>%
      mutate(
        write_path = glue("outputs/nma-otp/{outcome}-{timepoint}-{dose}.rds")
      )
  ),

  tar_target(write_dose_nma, {
    this_mod <-
      dose_nma %>%
      pluck(dose_nma_key$model_index)

    # write_rds(this_mod, dose_nma_key$write_path)
  },
  pattern = map(dose_nma_key)),


  tar_target(
    dose_nma_rel,
    dose_nma[[dose_nma_key$model_index]] %>%
      pluck('result') %>%
      extract_nma_estimates() %>%
      filter(par == "d") %>%
      rename(intervention = par_index) %>%
      select(-par) %>%
      bind_cols(dose_nma_key) %>%
      # needed to add dose here
      select(outcome,
             timepoint,
             intervention,
             dose,
             mean,
             sd,
             ci_lb,
             ci_ub) %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      mutate(
        mean = if_else(model_type == "lor",
                       exp(mean), mean),
        ci_lb = if_else(model_type == "lor",
                        exp(ci_lb), ci_lb),
        ci_ub = if_else(model_type == "lor",
                        exp(ci_ub), ci_ub)

      ) %>%
      rename_with(~ glue("nma_{.x}"),
                  c(mean, sd, ci_lb, ci_ub))
    ,
    pattern = map(dose_nma_key)
  ),

  tar_target(
    dose_nma_rank,
    dose_nma[[dose_nma_key$model_index]] %>%
      pluck("result") %>%
      posterior_ranks(lower_better = (
        outcome_label(dose_nma_key$outcome) == "lower"
      )) %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(
        intervention = parameter %>%
          str_remove("rank\\[") %>%
          str_remove("\\]"),
        rank_ci_lb = x2_5_percent,
        rank_ci_ub = x97_5_percent,
        rank_mean = mean
      ) %>%
      bind_cols(dose_nma_key) %>%
      # needed to add dose here
      select(outcome,
             timepoint,
             dose,
             intervention,
             starts_with("rank")),
    pattern = map(dose_nma_key)
  ),

  tar_target(
    dose_nma_rank_rel,
    dose_nma_key %>%
      select(-error) %>%
      rename(nma_studies = n_studies,
             nma_participants = participants) %>%
      left_join(dose_nma_rel, by = c("outcome", "timepoint", "dose")) %>%
      full_join(dose_nma_rank) %>%
      rename(nma_index = model_index)
  ),

  tar_target(
    dose_nma_results,
    dose_nma_rank_rel %>%
      select(outcome, intervention, timepoint, dose,
             everything())
  ),
  # outcome-level pw --------------------------------------------------------

  # set up each row for one pw analysis on
  # outcome, timepoint, and intervention
  tar_target(
    o_rma_groups,
    o_groups %>%
      mutate(# extract interventions
        intervention = map(
          dat,
          ~ .x %>% filter(intervention != "placebo") %>%
            pull(intervention) %>% levels()
        )) %>%
      unnest(intervention) %>%
      select(outcome, timepoint, intervention, dat) %>%
      ungroup()

  ),

  tar_target(
    o_rma_dat,
    o_rma_groups %>%
      mutate(
        # get studies with intervention (but not placebo)
        int_studies = map2(
          dat,
          intervention,
          ~ .x %>% filter(intervention == .y) %>%
            pull(study) %>% unique()
        ),
        # count how many studies match intervention
        int_study_n = map_int(int_studies, length)
      ) %>%
      filter(int_study_n > 1) %>%
      mutate(# get pw data for intervention
        pw_dat = pmap(
          list(dat,
               outcome,
               timepoint,
               intervention),
          .f = function(dat,
                        outcome,
                        timepoint,
                        intervention) {
            get_pw_dat(dat,
                       outcome,
                       timepoint,
                       intervention,
                       g2 = "placebo")
          }
        )) %>%
      mutate(# true number of studies that have placebo as well
        int_study_n = map_int(pw_dat,
                              ~ .x$study %>% n_distinct())) %>%
      filter(int_study_n > 1)
  ),



  tar_target(o_rma_init,
             {
               o_rma_dat %>%
                 mutate(n_studies = map_int(pw_dat, function(dat) {
                   dat %>%
                     pull(study) %>%
                     unique() %>%
                     length()
                 })) %>%
                 right_join(o_groups %>%
                              select(outcome, timepoint)) %>%
                 select(outcome, timepoint, intervention,
                        everything()) %>%
                 select(-dat)

             }),

  tar_target(
    o_rma,
    o_rma_init %>%
      mutate(pw_rma = map(pw_dat,
                          function(pw_dat) {
                            hpp_rma(pw_dat)
                          })),
    pattern = map(o_rma_init)
  ),

  tar_target(sof_acr,

             {
               baseline_acr_dat <-
                 mod_dat %>%
                 filter(# we don't have baseline for many
                   intervention == "placebo",
                   model_type == "lor",
                   timepoint == "baseline") %>%
                 group_by(outcome) %>%
                 summarise(acr_baseline = sum(r) / sum(n))


               other_acr <-
                 mod_dat %>%
                 filter(intervention == "placebo",
                        model_type == "lor") %>%
                 group_by(outcome) %>%
                 summarise(acr_crude = sum(r) / sum(n))

               other_acr %>%
                 left_join(baseline_acr_dat) %>%
                 mutate(acr = if_else(!is.na(acr_baseline),
                                      acr_baseline,
                                      acr_crude))


             }),

  tar_target(write_sof_acr,
             usethis::use_data(sof_acr, overwrite = T)),

  tar_target(
    o_rma_eggers,
    o_rma %>%
      mutate(
        regtest = map(pw_rma,
                      ~ .x$rma %>% safe_regtest()),
        reg_error = map(regtest, "error"),
        reg_error = map_int(reg_error, length)
      ) %>%
      filter(reg_error == 0) %>%
      select(outcome, timepoint, intervention,
             regtest) %>%
      mutate(
        estimate = map_dbl(regtest, ~ .x$result$est),
        ci_lb = map_dbl(regtest, ~ .x$result$ci.lb),
        ci_ub = map_dbl(regtest, ~ .x$result$ci.ub),
        z = map_dbl(regtest, ~ .x$result$zval),
        p = map_dbl(regtest, ~ .x$result$pval)

      )
  ),

  tar_target(
    o_results,
    full_join(o_nma_rank_rel,
              o_rma %>%
                rename(pw_studies = n_studies)) %>%
      right_join(o_nma_key) %>%
      rename(
        outcome_studies = n_studies,
        outcome_participants = participants,
        outcome_dat = dat
      )  %>%
      left_join(
        o_rma_eggers %>%
          select(outcome, timepoint, intervention,
                 regtest)
      ) %>%
      filter(!is.na(mean))
  ),


  # pw condition ------------------------------------------------------------

  # set up each row for one pw analysis on
  # outcome, timepoint, and intervention

  tar_target(con_rma_setup, {
    mod_dat %>%
      filter(timepoint != "follow_up",
             timepoint != "baseline") %>%
      group_by(outcome,
               timepoint,
               condition) %>%
      summarise(n_studies = n_distinct(study)) %>%
      distinct() %>%
      filter(timepoint != "followup") %>%
      mutate(
        dat = pmap(
          list(o = outcome,
               tp = timepoint,
               condition),
          .f = function(o, tp, subgroup) {
            mod_dat %>%
              filter(outcome == o,
                     timepoint == tp,
                     condition == subgroup) %>%
              viable_observations()

          }

        ),
        participants = map_int(dat,
                               ~ sum(.x$n)),
        placebo_check = map_lgl(dat,
                                ~ any("placebo" == .x$type))
      ) %>%
      # this should be documented
      filter(placebo_check,
             participants > 200) %>%
      select(-placebo_check)
  }),

  tar_target(
    con_rma_groups,
    con_rma_setup %>%
      mutate(# extract interventions
        intervention = map(
          dat,
          ~ .x %>% filter(intervention != "placebo") %>%
            pull(intervention) %>% levels()
        )) %>%
      unnest(intervention) %>%
      select(outcome, timepoint, intervention, condition, dat) %>%
      ungroup()

  ),

  tar_target(
    con_rma_dat,
    con_rma_groups %>%
      mutate(
        # get studies with intervention (but not placebo)
        int_studies = map2(
          dat,
          intervention,
          ~ .x %>% filter(intervention == .y) %>%
            pull(study) %>% unique()
        ),
        # count how many studies match intervention
        int_study_n = map_int(int_studies, length)
      ) %>%
      filter(int_study_n > 1) %>%
      mutate(# get pw data for intervention
        pw_dat = pmap(
          list(dat,
               outcome,
               timepoint,
               intervention),
          .f = function(dat,
                        outcome,
                        timepoint,
                        intervention) {
            get_pw_dat(dat,
                       outcome,
                       timepoint,
                       intervention,
                       g2 = "placebo")
          }
        )) %>%
      mutate(# true number of studies that have placebo as well
        int_study_n = map_int(pw_dat,
                              ~ .x$study %>% n_distinct())) %>%
      filter(int_study_n > 1)
  ),



  tar_target(con_rma_init,
             {
               con_rma_dat %>%
                 mutate(n_studies = map_int(pw_dat, function(dat) {
                   dat %>%
                     pull(study) %>%
                     unique() %>%
                     length()
                 })) %>%
                 right_join(o_groups %>%
                              select(outcome, timepoint)) %>%
                 select(outcome, timepoint, intervention,
                        everything()) %>%
                 select(-dat)

             }),

  tar_target(
    con_rma,
    con_rma_init %>%
      mutate(pw_rma = map(pw_dat,
                          function(pw_dat) {
                            hpp_rma(pw_dat)
                          })),
    pattern = map(con_rma_init)
  ),

  tar_target(con_rma_sof_acr,

             {
               baseline_acr_dat <-
                 mod_dat %>%
                 filter(# we don't have baseline for many
                   intervention == "placebo",
                   model_type == "lor",
                   timepoint == "baseline") %>%
                 group_by(outcome) %>%
                 summarise(acr_baseline = sum(r) / sum(n))


               other_acr <-
                 mod_dat %>%
                 filter(intervention == "placebo",
                        model_type == "lor") %>%
                 group_by(outcome) %>%
                 summarise(acr_crude = sum(r) / sum(n))

               other_acr %>%
                 left_join(baseline_acr_dat) %>%
                 mutate(acr = if_else(!is.na(acr_baseline),
                                      acr_baseline,
                                      acr_crude))


             }),


  tar_target(
    con_rma_eggers,
    con_rma %>%
      mutate(
        regtest = map(pw_rma,
                      ~ .x$rma %>% safe_regtest()),
        reg_error = map(regtest, "error"),
        reg_error = map_int(reg_error, length)
      ) %>%
      filter(reg_error == 0) %>%
      select(outcome, timepoint, intervention, condition,
             regtest) %>%
      mutate(
        estimate = map_dbl(regtest, ~ .x$result$est),
        ci_lb = map_dbl(regtest, ~ .x$result$ci.lb),
        ci_ub = map_dbl(regtest, ~ .x$result$ci.ub),
        z = map_dbl(regtest, ~ .x$result$zval),
        p = map_dbl(regtest, ~ .x$result$pval)

      )
  ),

  tar_target(
    con_rma_results,
    con_rma %>%
      left_join(
        con_rma_eggers %>%
          select(outcome, timepoint, intervention, condition,
                 regtest)
      ) %>%
      filter(!is.na(mean)) %>%
      arrange(outcome, timepoint, condition, intervention) %>%
      filter(intervention != "placebo")
  ),




  # sof ---------------------------------------------------------------------




  tar_target(
    sof_results,
    sof_results_dat(nma_mod_dev) %>%
      left_join(outcome_key)
  ),

  tar_target(
    sof_acr_dev,
    sof_results %>%
      left_join(sof_acr) %>%
      filter(intervention != "placebo") %>%
      select(outcome, intervention, acr, mean,
             direction_of_improvement) %>%
      mutate(rd = map2_dbl(mean, acr, calc_rd_par),
             nnt = 1 / abs(rd))
  ),



  tar_target(sof_acr_fn_dev, {
    this_outcome <-
      sof_results %>%
      pull(outcome) %>% unique()
    this_acr <-
      sof_acr %>%
      filter(outcome == this_outcome) %>%
      pull(acr) %>% unique()
    sof_nnt(sof_results, this_acr)
    # this_outcome

  }),



  # nnt ---------------------------------------------------------------------

  tar_target(
    nnt_dat,
    o_nma_results %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      filter(model_type == "lor") %>%
      select(outcome, timepoint, intervention, dat, nma_mean) %>%
      mutate(
        int_dat =
          map2(dat,
               intervention,
               function(dat, int) {
                 int_studies <-
                   dat %>%
                   filter(intervention == int) %>%
                   pull(study) %>% unique()
                 dat %>%
                   filter(intervention %in% c("placebo", int),
                          study %in% int_studies) %>%
                   # viable_observations() %>%
                   mutate(
                     is_placebo = intervention == "placebo",
                     is_placebo =
                       if_else(is_placebo, "control", "int")
                   )
               }),

        obs = map_int(int_dat, nrow)
      )
  ),

  tar_target(
    nnt_groups,
    nnt_dat %>%
      mutate(rates = map(
        int_dat,
        .f = function(df) {
          df %>%
            group_by(is_placebo) %>%
            summarise(rate = sum(r),
                      n = sum(n)) %>%
            pivot_wider(
              names_from = is_placebo,
              values_from = c(rate, n),
              names_glue = "{is_placebo}_{.value}"
            ) %>%
            mutate(n_studies = n_distinct(df$study),
                   n_total = sum(df$n))

        }
      )) %>%
      unnest(rates) %>%
      mutate(
        control_risk = control_rate / control_n,
        arr = control_risk - int_rate / int_n,
        nnt = 1 / abs(arr),
        nnt_direct = ceiling(nnt) %>% as.integer()
      )

  ),

  tar_target(
    o_nma_nnt,
    nnt_groups %>%
      left_join(sof_acr) %>%
      mutate(
        acr = if_else(is.na(control_risk), acr, control_risk),
        acr_rd = map2_dbl(nma_mean,
                          acr,
                          calc_rd_par),
        # nnt_or = map2_dbl(nma_mean, acr, nnt),
        nnt_or = ceiling(1 / abs(acr_rd)),
        nnt_aae = 1000 * acr_rd
      )
  ),

  tar_target(
    nnt_results,
    o_nma_nnt %>%
      select(
        outcome,
        timepoint,
        intervention,
        control_risk,
        nnt_direct,
        nnt_or,
        nnt_aae
      ) %>%
      left_join(intervention_key) %>%
      mutate(class_label =
               ifelse(class == "n/a",
                      NA,
                      toupper(class))) %>%
      left_join(
        nnt_groups %>% select(outcome,
                              timepoint,
                              intervention,
                              n_total,
                              n_studies)
      ) %>%
      select(
        outcome,
        timepoint,
        intervention,
        class = class_label,
        type,
        nnt = nnt_or,
        participants = n_total,
        studies = n_studies,
        nnt_aae
      )
  ),


  tar_target(
    nnt_wide,
    nnt_results %>%
      # filter(participants > 200) %>%
      pivot_wider(
        id_cols = c(timepoint, intervention, class, type),
        names_from = outcome,
        values_from = c(nnt, participants, studies),
        names_glue = "{outcome}_{.value}"
      ) %>%
      arrange(desc(pain_sub_nnt)) %>%
      select(
        intervention,
        timepoint,
        class,
        type,
        starts_with("pain_sub"),
        starts_with("pain_mod"),
        starts_with("pgic_any_improvement"),
        starts_with("pgic_much_or_very_much_improved"),
        starts_with("withdrawal"),
        starts_with("adverse_dropout"),
        starts_with("adverse"),
        starts_with("serious_adverse"),
        starts_with("physical")
      )

  ),


  # pw nnt ------------------------------------------------------------------

  tar_target(
    nnt_pw_dat,
    o_results %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      mutate(rma_mean =
               map_dbl(pw_rma,
                       ~.x$rma$beta %>% as.numeric())) %>%
      filter(model_type == "lor") %>%
      select(outcome, timepoint, intervention, dat, rma_mean) %>%
      mutate(
        int_dat =
          map2(dat,
               intervention,
               function(dat, int) {
                 int_studies <-
                   dat %>%
                   filter(intervention == int) %>%
                   pull(study) %>% unique()
                 dat %>%
                   filter(intervention %in% c("placebo", int),
                          study %in% int_studies) %>%
                   # viable_observations() %>%
                   mutate(
                     is_placebo = intervention == "placebo",
                     is_placebo =
                       if_else(is_placebo, "control", "int")
                   )
               }),

        obs = map_int(int_dat, nrow)
      )
  ),

  tar_target(
    nnt_pw_groups,
    nnt_pw_dat %>%
      mutate(rates = map(
        int_dat,
        .f = function(df) {
          df %>%
            group_by(is_placebo) %>%
            summarise(rate = sum(r),
                      n = sum(n)) %>%
            pivot_wider(
              names_from = is_placebo,
              values_from = c(rate, n),
              names_glue = "{is_placebo}_{.value}"
            ) %>%
            mutate(n_studies = n_distinct(df$study),
                   n_total = sum(df$n))

        }
      )) %>%
      unnest(rates) %>%
      mutate(
        control_risk = control_rate / control_n,
        arr = control_risk - int_rate / int_n,
        nnt = 1 / abs(arr),
        nnt_pw_direct = ceiling(nnt) %>% as.integer()
      )

  ),

  tar_target(
    nnt_pw_nma,
    nnt_pw_groups %>%
      left_join(sof_acr) %>%
      mutate(
        acr = if_else(is.na(control_risk), acr, control_risk),
        acr_rd = map2_dbl(rma_mean,
                          acr,
                          calc_rd_par),
        # nnt_pw_or = map2_dbl(nma_mean, acr, nnt),
        nnt_pw_or = ceiling(1 / abs(acr_rd)),
        nnt_aae = 1000 * acr_rd
      )
  ),

  tar_target(
    nnt_pw_results,
    o_nma_nnt %>%
      select(
        outcome,
        timepoint,
        intervention,
        control_risk,
        nnt_pw_direct,
        nnt_pw_or
      ) %>%
      left_join(intervention_key) %>%
      mutate(class_label =
               ifelse(class == "n/a",
                      NA,
                      toupper(class))) %>%
      left_join(
        nnt_pw_groups %>% select(outcome,
                                 timepoint,
                                 intervention,
                                 n_total,
                                 n_studies)
      ) %>%
      select(
        outcome,
        timepoint,
        intervention,
        class = class_label,
        type,
        nnt = nnt_pw_or,
        participants = n_total,
        studies = n_studies
      )
  ),


  tar_target(
    nnt_pw_wide,
    nnt_pw_results %>%
      # filter(participants > 200) %>%
      pivot_wider(
        id_cols = c(timepoint, intervention, class, type),
        names_from = outcome,
        values_from = c(nnt, participants, studies),
        names_glue = "{outcome}_{.value}"
      ) %>%
      arrange(desc(pain_sub_nnt)) %>%
      select(
        intervention,
        timepoint,
        class,
        type,
        starts_with("pain_sub"),
        starts_with("pain_mod"),
        starts_with("pgic_any_improvement"),
        starts_with("pgic_much_or_very_much_improved"),
        starts_with("adverse_dropout"),
        starts_with("withdrawal"),
        starts_with("adverse"),
        starts_with("serious_adverse"),
        starts_with("physical")
      )

  ),

  # nnt condition -----------------------------------------------------------

  tar_target(
    nnt_con_dat,
    con_nma_results %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      filter(model_type == "lor") %>%
      select(outcome, timepoint, condition, intervention, dat, nma_mean) %>%
      mutate(
        int_dat =
          map2(dat,
               intervention,
               function(dat, int) {
                 int_studies <-
                   dat %>%
                   filter(intervention == int) %>%
                   pull(study) %>% unique()

                 dat %>%
                   filter(intervention %in% c("placebo", int),
                          study %in% int_studies) %>%
                   viable_observations() %>%
                   ungroup()
               }),

        obs = map_int(int_dat, nrow)
      ) %>%
      filter(obs > 0)
  ),

  tar_target(
    nnt_con_groups,
    nnt_con_dat %>%
      mutate(rates = map(
        int_dat,
        .f = function(df) {
          df %>%
            mutate(is_placebo =
                     if_else(intervention == 'placebo', "control", "int")) %>%
            group_by(is_placebo) %>%
            summarise(rate = sum(r),
                      n = sum(n)) %>%
            pivot_wider(
              names_from = is_placebo,
              values_from = c(rate, n),
              names_glue = "{is_placebo}_{.value}"
            ) %>%
            mutate(n_studies = n_distinct(df$study),
                   n_total = sum(df$n))

        }
      )) %>%
      unnest(rates) %>%
      mutate(
        control_risk = control_rate / control_n,
        arr = control_risk - int_rate / int_n,
        nnt = 1 / abs(arr),
        nnt_con_direct = ceiling(nnt) %>% as.integer()
      )

  ),

  tar_target(
    nnt_con_calc,
    nnt_con_groups %>%
      left_join(sof_acr) %>%
      mutate(
        acr = if_else(is.na(control_risk), acr, control_risk),
        acr_rd = map2_dbl(nma_mean,
                          acr,
                          calc_rd_par),
        # nnt_con_or = map2_dbl(nma_mean, acr, nnt),
        nnt_con_or = ceiling(1 / abs(acr_rd)),
        nnt_aae = 1000 * acr_rd
      )
  ),

  tar_target(
    nnt_con_results,
    nnt_con_calc %>%
      select(
        outcome,
        timepoint,
        condition,
        intervention,
        control_risk,
        nnt_con_direct,
        nnt_con_or
      ) %>%
      left_join(intervention_key) %>%
      mutate(class_label =
               ifelse(class == "n/a",
                      NA,
                      toupper(class))) %>%
      left_join(
        nnt_con_groups %>% select(outcome,
                                  timepoint,
                                  intervention,
                                  condition,
                                  n_total,
                                  n_studies)
      ) %>%
      select(
        outcome,
        timepoint,
        intervention,
        condition,
        class = class_label,
        type,
        nnt = nnt_con_or,
        participants = n_total,
        studies = n_studies
      )
  ),


  tar_target(
    nnt_con_wide,
    nnt_con_results %>%
      # filter(participants > 200) %>%
      pivot_wider(
        id_cols = c(timepoint, intervention, class, type, condition),
        names_from = outcome,
        values_from = c(nnt, participants, studies),
        names_glue = "{outcome}_{.value}"
      ) %>%
      arrange(desc(pain_sub_nnt)) %>%
      select(
        intervention,
        timepoint,
        condition,
        class,
        type,
        starts_with("pain_sub"),
        starts_with("pain_mod"),
        starts_with("pgic_much_or_very_much_improved"),
        starts_with("pgic_any_improvement"),
        starts_with("withdrawal"),
        starts_with("adverse"),
        starts_with("adverse_dropout"),
        starts_with("serious_adverse"),
        starts_with("physical")
      )

  ),

  # nnt dose  -----------------------------------------------------------

  tar_target(
    nnt_dose_dat,
    dose_nma_results %>%
      left_join(outcome_key %>% select(outcome, model_type)) %>%
      filter(model_type == "lor") %>%
      select(outcome, timepoint, dose, intervention, dat, nma_mean) %>%
      mutate(
        int_dat =
          map2(dat,
               intervention,
               function(dat, int) {
                 int_studies <-
                   dat %>%
                   filter(intervention == int) %>%
                   pull(study) %>% unique()

                 dat %>%
                   filter(intervention %in% c("placebo", int),
                          study %in% int_studies) %>%
                   viable_observations() %>%
                   ungroup()
               }),

        obs = map_int(int_dat, nrow)
      ) %>%
      filter(obs > 0)
  ),

  tar_target(
    nnt_dose_groups,
    nnt_dose_dat %>%
      mutate(rates = map(
        int_dat,
        .f = function(df) {
          df %>%
            mutate(is_placebo =
                     if_else(intervention == 'placebo', "dosetrol", "int")) %>%
            group_by(is_placebo) %>%
            summarise(rate = sum(r),
                      n = sum(n)) %>%
            pivot_wider(
              names_from = is_placebo,
              values_from = c(rate, n),
              names_glue = "{is_placebo}_{.value}"
            ) %>%
            mutate(n_studies = n_distinct(df$study),
                   n_total = sum(df$n))

        }
      )) %>%
      unnest(rates) %>%
      mutate(
        dosetrol_risk = dosetrol_rate / dosetrol_n,
        arr = dosetrol_risk - int_rate / int_n,
        nnt = 1 / abs(arr),
        nnt_dose_direct = ceiling(nnt) %>% as.integer()
      )

  ),

  tar_target(
    nnt_dose_calc,
    nnt_dose_groups %>%
      left_join(sof_acr) %>%
      mutate(
        acr = if_else(is.na(dosetrol_risk), acr, dosetrol_risk),
        acr_rd = map2_dbl(nma_mean,
                          acr,
                          calc_rd_par),
        # nnt_dose_or = map2_dbl(nma_mean, acr, nnt),
        nnt_dose_or = ceiling(1 / abs(acr_rd)),
        nnt_aae = 1000 * acr_rd
      )
  ),

  tar_target(
    nnt_dose_results,
    nnt_dose_calc %>%
      select(
        outcome,
        timepoint,
        dose,
        intervention,
        dosetrol_risk,
        nnt_dose_direct,
        nnt_dose_or
      ) %>%
      left_join(intervention_key) %>%
      mutate(class_label =
               ifelse(class == "n/a",
                      NA,
                      toupper(class))) %>%
      left_join(
        nnt_dose_groups %>% select(outcome,
                                   timepoint,
                                   intervention,
                                   dose,
                                   n_total,
                                   n_studies)
      ) %>%
      select(
        outcome,
        timepoint,
        intervention,
        dose,
        class = class_label,
        type,
        nnt = nnt_dose_or,
        participants = n_total,
        studies = n_studies
      )
  ),


  tar_target(
    nnt_dose_wide,
    nnt_dose_results %>%
      # filter(participants > 200) %>%
      pivot_wider(
        id_cols = c(timepoint, intervention, class, type, dose),
        names_from = outcome,
        values_from = c(nnt, participants, studies),
        names_glue = "{outcome}_{.value}"
      ) %>%
      arrange(desc(pain_sub_nnt)) %>%
      select(
        intervention,
        timepoint,
        dose,
        class,
        type,
        starts_with("pain_sub"),
        starts_with("pain_mod"),
        starts_with("pgic_much_or_very_much_improved"),
        starts_with("pgic_any_improvement"),
        starts_with("withdrawal"),
        starts_with("adverse"),
        starts_with("adverse_dropout"),
        starts_with("serious_adverse"),
        starts_with("physical")
      )

  ),

  # reporting datasets ------------------------------------------------------


  # sof ---------------------------------------------------------------------


  tar_target(
    oti_int,
    o_nma_rel %>%
      select(outcome, timepoint, intervention) %>%
      left_join(
        o_nma_key %>%
          select(
            outcome,
            timepoint,
            dat,
            studies_outcome = n_studies,
            n_outcome = participants
          )
      ) %>%
      mutate(
        studies =
          map2(
            dat,
            intervention,
            ~ .x %>% filter(intervention == .y) %>% pull(study) %>% unique()
          ),
        studies_int = map_int(studies, length),
        n_total = pmap_dbl(
          list(intn = intervention,
               df = dat,
               st = studies),
          .f = function(intn, df, st) {
            df %>%
              dplyr::filter(intervention %in% c("placebo", intn),
                            study %in% st) %>%
              pull(n) %>% sum()
          }

        ),
        n_placebo = map2_dbl(dat, studies, ~.x %>% filter(intervention == "placebo", study %in% .y) %>% pull(n) %>% sum())
      ) %>%
      select(contains('studies'), starts_with("n_"), everything())

  ),

  tar_target(
    oti_results,
    nnt_results %>%
      select(outcome,
             timepoint,
             intervention,
             # nnt_direct,
             # nnt_or
             nnt,
             nnt_aae) %>%
      right_join(o_results) %>%
    left_join(oti_int)
      ),

  tar_target(
    oti_pw,
    # filter out failed rma
    o_results %>%
      mutate(
        rma_class = map_chr(pw_rma, class),
        rma_length = map_int(pw_rma, length),
        rma_named = map_lgl(pw_rma, ~ !is.null(names(.x)))
      ) %>%
      select(
        outcome,
        timepoint,
        intervention,
        pw_rma,
        starts_with("rma_"),
        everything()
      ) %>%
      filter(rma_class == "list") %>%
      select(-rma_class) %>%
      mutate(
        pw_mean = map_dbl(pw_rma,
                          ~ .x$rma_mv$beta),
        pw_mean = if_else(model_type == "lor",
                          exp(pw_mean),
                          pw_mean),
        pw_i_sq = map_dbl(pw_rma,
                          ~ .x$rma$I2),
        pw_tau_sq = map_dbl(pw_rma,
                            ~ .x$rma_mv$tau2)
      ) %>%
      left_join(intervention_key) %>%

      filter(n_total > 200) %>%
      mutate(
        intervention = fct_relevel(intervention,
                                   "duloxetine",
                                   "amitriptyline",
                                   "milnacipran")
      ) %>%
      arrange(intervention)
  ),

  tar_target(
    oti_sof,
    oti_results %>%
      mutate(
        rank = glue(
          "{round(rank_mean)} ({round(rank_ci_lb)} to {round(rank_ci_ub)})"
        ),
        rel = glue(
          "{round(nma_mean, 2)} ({round(nma_ci_lb,2)} to {round(nma_ci_ub, 2)}) "
        )
      ) %>%
      left_join(
        oti_int %>%
          select(outcome, timepoint, intervention, n_total, studies_int)
      ) %>%
      mutate(
        n = n_total / outcome_participants,
        n = round(n, 2),
        studies = studies_int / outcome_studies,
        studies = round(studies, 2)
      ) %>%
      left_join(rob) %>%
      mutate(high_risk_p = round(high_risk_p, 2)) %>%
      select(
        outcome,
        timepoint,
        intervention,
        RCT = studies_int,
        studies,
        participants = n_total,
        n,
        rel,
        nnt,
        rank,
        high_risk_p
      ) %>%
      # filter(participants > 200) %>%
      arrange(outcome, timepoint, intervention) %>%
      mutate(
        intervention = fct_relevel(intervention, "duloxetine", "amitriptyline", "milnacipran")
      )

  ),


  # rob reporting -----------------------------------------------------------

  tar_target(
    rob_output,
    mod_dat %>% select(
      study,
      sequence_generation,
      allocation_concealment,
      blinding_of_participants_and_personnel,
      blinding_of_outcome_assessors,
      incomplete_outcome_data,
      selective_outcome_reporting,
      other_sources_of_bias,
      rob
    ) %>% distinct()
  ),


  # diff: reboot from mod dat for smd comparisons ---------------------------------
  # keys --------------------------------------------------------------------

  # select overall keys down to what is used in this analysis

  tar_target(
    diff_outcome_key,
    outcome_key %>%
      select(outcome,
             outcome_label,
             model_type)
  ),

  tar_target(
    diff_int_key,
    intervention_key %>%

      # only get more granular if required
      # might need antidepressant key at some point
      mutate(
        class = ifelse(class == "n/a",
                       NA,
                       class),
        class_label = ifelse(!is.na(class),
                             toupper(class),
                             "")
      )
  ),

  # model obs ---------------------------------------------------------------

  # select overall keys down to what is used in this analysis

  tar_target(
    diff_obs,
    mod_dat %>%
      select(
        outcome,
        timepoint,
        condition,
        dose,
        arm,
        intervention,
        study,
        mean,
        sd,
        se,
        r,
        n,
        model_type
      ) %>%
      filter(str_detect(timepoint, "change_score|post_int")) %>%
      group_by(outcome, timepoint) %>%
      distinct()
  ),


  tar_target(
    diff_obs_wide_length,
    diff_obs %>%
      select(-condition,-dose) %>%
      mutate(is_placebo = if_else(
        intervention == "placebo",
        "placebo",
        "int"
      )) %>%
      pivot_wider(
        id_cols = c(outcome, timepoint, arm, intervention, study),
        values_from = c(mean, sd, se, r),
        names_from = is_placebo,
        names_glue = "{.value}_{is_placebo}",
        values_fn = length
      )
  ),

  tar_target(
    diff_placebo_duplicates,
    diff_obs_wide_length %>%
      filter(mean_placebo > 1 |
               r_placebo > 1) %>%
      select(outcome, timepoint,
             # arm,
             study) %>%
      left_join(diff_obs)
  ),

  # placebo target

  tar_target(
    diff_placebo,
    diff_placebo_duplicates %>%
      filter(
        round(mean) != 39 &
          study != "tetreault 2018" & intervention != "placebo"
      ) %>%
      anti_join(diff_obs, .) %>%
      filter(intervention == "placebo") %>%
      select(-condition,-dose) %>%
      rename_with( ~ glue("{.x}_placebo"),
                   mean:n) %>%
      select(-arm, -intervention)
  ),


  # intervention targets

  tar_target(
    diff_intn,
    diff_obs %>%
      filter(intervention != "placebo") %>%
      rename_with( ~ glue("{.x}_intn"),
                   mean:n)
  ),

  # join together

  tar_target(
    diff_wide,
    left_join(diff_intn, diff_placebo) %>%
      mutate(model_type = map_chr(outcome, outcome_mod))
  ),

  # model data

  tar_target(
    diff_obs_smd,
    diff_wide %>%
      filter(model_type == "smd") %>%
      select(-starts_with("r_"), -model_type) %>%
      mutate(n_smd = n_intn + n_placebo)
  ),

  tar_target(
    diff_obs_lor,
    diff_wide %>%
      filter(model_type == "lor") %>%
      select(
        -starts_with("mean"),-starts_with("sd"),-model_type,
        -starts_with("se")
      ) %>%
      mutate(n_smd = n_intn + n_placebo)
  ),

  # check that each arm has placebo
  tar_target(
    diff_wide_check_smd,
    diff_obs_smd %>%
      filter(is.na(mean_placebo)) %>%
      filter(!str_detect(outcome, "adverse"),
             outcome != "withdrawal") %>%
      select(outcome, timepoint, study, arm)
  ),

  tar_target(
    diff_wide_check_lor,
    diff_obs_lor %>%
      filter(is.na(r_placebo)) %>%
      filter(!str_detect(outcome, "adverse"),
             outcome != "withdrawal") %>%
      select(outcome, timepoint, study, arm)
  ),


  # models ------------------------------------------------------------------


  # outcome pairwise --------------------------------------------------------


  tar_target(
    diff_smd_groups,
    diff_obs_smd %>%
      # anti_join(diff_obs_smd, diff_wide_check_smd) %>%
      # filter(!is.na(n_placebo)) %>%
      group_by(outcome, timepoint, intervention) %>%
      filter(# sum(n_smd) > 200,
        length(study) > 1) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    diff_smd_escalc,
    {
      escalc(
        measure = "SMD",
        # m1i - m2i is calculed
        m1i = mean_intn,
        sd1i = sd_intn,
        n1i = n_intn,
        m2i = mean_placebo,
        sd2i = sd_placebo,
        n2i = n_placebo,
        data = diff_smd_groups
      ) %>%
        select(outcome, timepoint, intervention, arm, study, yi, vi) %>%
        mutate(model_type = "smd") %>%
        full_join(
          diff_smd_groups %>%
            select(
              outcome,
              timepoint,
              intervention,
              study,
              arm,
              n_intn,
              n_placebo
            )

        )

    },
    pattern = map(diff_smd_groups),
    iteration = "list"
  ),

  tar_target(
    diff_lor_groups,
    diff_obs_lor %>%
      # anti_join(diff_obs_lor, diff_wide_check_lor) %>%
      # need to investigate studies with no placebo
      # filter(!is.na(n_placebo)) %>%
      group_by(outcome, timepoint, intervention) %>%
      filter(# sum(n_smd) > 200,
        length(study) > 1) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    diff_lor_escalc,
    {
      escalc(
        measure = "OR",
        # log of (ai*di) / (bi*ci)
        ai = r_intn,
        n1i = n_intn,
        ci = r_placebo,
        n2i = n_placebo,
        data = diff_lor_groups
      ) %>%
        select(outcome,
               timepoint,
               intervention,
               arm, study, yi, vi) %>%
        mutate(model_type = "lor") %>%
        full_join(
          diff_lor_groups %>%
            select(
              outcome,
              timepoint,
              intervention,
              study,
              arm,
              n_intn,
              n_placebo
            )

        )
    },
    pattern = map(diff_lor_groups),
    iteration = "list"
  ),


  # rma ---------------------------------------------------------------------

  tar_target(
    diff_smd_rma,
    rma(
      yi = yi,
      vi = vi,
      measure = "SMD",
      slab = study,
      data = diff_smd_escalc
    ),
    pattern = map(diff_smd_escalc),
    iteration = "list"
  ),

  tar_target(
    diff_lor_rma,
    rma(
      yi = yi,
      vi = vi,
      measure = "OR",
      slab = study,
      data = diff_lor_escalc
    ),
    pattern = map(diff_lor_escalc),
    iteration = "list"
  ),

  tar_target(
    diff_rma_smd_key,
    diff_smd_groups %>%
      select(outcome, timepoint, intervention, tar_group, starts_with("n_")) %>%
      group_by(outcome, timepoint, intervention, tar_group) %>%
      summarise(
        n_smd_placebo = sum(n_placebo),
        n_smd_itnn = sum(n_intn),
        n_smd = sum(n_smd)
      ) %>%
      arrange(tar_group) %>%
      ungroup() %>%
      mutate(dat = diff_smd_escalc,
             rma = diff_smd_rma) %>%
      select(-tar_group)
  ),

  tar_target(
    diff_rma_lor_key,
    diff_lor_groups %>%
      select(outcome, timepoint, intervention, tar_group, starts_with("n_")) %>%
      group_by(outcome, timepoint, intervention, tar_group) %>%
      summarise(
        n_smd_placebo = sum(n_placebo),
        n_smd_itnn = sum(n_intn),
        n_smd = sum(n_smd)
      ) %>%
      arrange(tar_group) %>%
      ungroup() %>%
      mutate(dat = diff_lor_escalc,
             rma = diff_lor_rma) %>%
      select(-tar_group)
  ),

  tar_target(
    diff_rma_key,
    bind_rows(diff_rma_smd_key, diff_rma_lor_key) %>%
      mutate(regtest = map(rma, safe_regtest))
  ),


  # pw conditions -----------------------------------------------------------


  tar_target(
    diff_con_smd_groups,
    anti_join(diff_obs_smd, diff_wide_check_smd) %>%
      filter(!is.na(n_placebo)) %>%
      group_by(outcome, timepoint, condition, intervention) %>%
      filter(sum(n_smd) > 200, length(study) > 1) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    diff_con_smd_escalc,
    {
      escalc(
        measure = "SMD",
        # m1i - m2i is calculed
        m1i = mean_intn,
        sd1i = sd_intn,
        n1i = n_intn,
        m2i = mean_placebo,
        sd2i = sd_placebo,
        n2i = n_placebo,
        data = diff_con_smd_groups
      ) %>%
        select(outcome, timepoint, condition, intervention, arm, study, yi, vi) %>%
        mutate(model_type = "smd") %>%
        full_join(
          diff_con_smd_groups %>%
            select(
              outcome,
              timepoint,
              condition,
              intervention,
              study,
              arm,
              n_intn,
              n_placebo
            )

        )

    },
    pattern = map(diff_con_smd_groups),
    iteration = "list"
  ),

  tar_target(
    diff_con_lor_groups,
    anti_join(diff_obs_lor, diff_wide_check_lor) %>%
      # need to investigate studies with no placebo
      filter(!is.na(n_placebo)) %>%
      group_by(outcome, timepoint, condition, intervention) %>%
      filter(sum(n_smd) > 200, length(study) > 1) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    diff_con_lor_escalc,
    {
      escalc(
        measure = "OR",
        # log of (ai*di) / (bi*ci)
        ai = r_intn,
        n1i = n_intn,
        ci = r_placebo,
        n2i = n_placebo,
        data = diff_con_lor_groups
      ) %>%
        select(outcome,
               timepoint, condition,
               intervention,
               arm, study, yi, vi) %>%
        mutate(model_type = "lor") %>%
        full_join(
          diff_con_lor_groups %>%
            select(
              outcome,
              timepoint,
              condition,
              intervention,
              study,
              arm,
              n_intn,
              n_placebo
            )

        )
    },
    pattern = map(diff_con_lor_groups),
    iteration = "list"
  ),


  # rma ---------------------------------------------------------------------

  tar_target(
    diff_con_smd_rma,
    rma(
      yi = yi,
      vi = vi,
      measure = "SMD",
      slab = study,
      data = diff_con_smd_escalc
    ),
    pattern = map(diff_con_smd_escalc),
    iteration = "list"
  ),

  tar_target(
    diff_con_lor_rma,
    rma(
      yi = yi,
      vi = vi,
      measure = "OR",
      slab = study,
      data = diff_con_lor_escalc
    ),
    pattern = map(diff_con_lor_escalc),
    iteration = "list"
  ),

  tar_target(
    diff_con_rma_smd_key,
    diff_con_smd_groups %>%
      select(
        outcome,
        timepoint,
        condition,
        intervention,
        tar_group,
        starts_with("n_")
      ) %>%
      group_by(outcome, timepoint, condition, intervention, tar_group) %>%
      summarise(
        n_smd_placebo = sum(n_placebo),
        n_smd_itnn = sum(n_intn),
        n_smd = sum(n_smd)
      ) %>%
      arrange(tar_group) %>%
      ungroup() %>%
      mutate(dat = diff_con_smd_escalc,
             rma = diff_con_smd_rma) %>%
      select(-tar_group)
  ),

  tar_target(
    diff_con_rma_lor_key,
    diff_con_lor_groups %>%
      select(
        outcome,
        timepoint,
        condition,
        intervention,
        tar_group,
        starts_with("n_")
      ) %>%
      group_by(outcome, timepoint, condition, intervention, tar_group) %>%
      summarise(
        n_smd_placebo = sum(n_placebo),
        n_smd_itnn = sum(n_intn),
        n_smd = sum(n_smd)
      ) %>%
      arrange(tar_group) %>%
      ungroup() %>%
      mutate(dat = diff_con_lor_escalc,
             rma = diff_con_lor_rma) %>%
      select(-tar_group)
  ),

  tar_target(
    diff_con_rma_key,
    bind_rows(diff_con_rma_smd_key, diff_con_rma_lor_key)
  ),


  # nma ---------------------------------------------------------------------

  tar_target(
    diff_nma_groups,
    diff_obs %>%
      group_by(outcome, timepoint) %>%
      arrange(outcome, timepoint) %>%
      tar_group(),
    iteration = "group"

  ),

  tar_target(
    diff_nma_dev,
    diff_nma_groups %>%
      filter(tar_group == 2) %>%
      viable_observations() %>%
      set_agd_arm(
        study = study,
        trt = intervention,
        y = mean,
        se = se,
        sample_size = n,
        trt_ref = "placebo"
      )
  ),


  tar_target(
    diff_nma_net_smd,
    diff_nma_groups %>%
      filter(model_type == "smd") %>%
      mutate(model_type = map_chr(outcome, outcome_mod)) %>%
      viable_observations() %>%
      set_agd_arm(
        study = study,
        trt = intervention,
        y = mean,
        se = se,
        sample_size = n,
        trt_ref = "placebo"
      ),
    pattern = map(diff_nma_groups),
    iteration = "list"
  ),

  tar_target(
    diff_nma_net_lor,
    diff_nma_groups %>%
      mutate(model_type = map_chr(outcome, outcome_mod)) %>%
      filter(model_type == "lor") %>%
      viable_observations() %>%
      set_agd_arm(
        study = study,
        trt = intervention,
        r = r,
        n = n,
        sample_size = n,
        trt_ref = "placebo"
      ),
    pattern = map(diff_nma_groups),
    iteration = "list"
  ),


  tar_target(
    diff_nma_net,
    diff_nma_groups %>%
      select(outcome, timepoint, tar_group, model_type) %>%
      ungroup() %>%
      distinct()
    # %>%
    #   mutate(
    #     net = map(model_type,
    #               ~ifelse(
    #       .x == "lor",
    #       diff_nma_net_lor,
    #       diff_nma_net_smd
    #     )
    #
    #
    #     )

    ,
    pattern = map(diff_nma_net_lor, diff_nma_net_smd)
  ),

  # results -----------------------------------------------------------------





  # outcome -----------------------------------------------------------------

  tar_target(diff_outcome,
             diff_outcome_key),

  # outcome - intervention --------------------------------------------------

  tar_target(diff_int,
             diff_int_key),



  # md or smd ---------------------------------------------------------------

  tar_target(md_smd_check,
             oti_results),



  # by outcome smd ----------------------------------------------------------

  tar_target(
    smd_mood_cs,
    mod_dat %>%
      filter(outcome == "mood",
             timepoint == "change_score") %>%
      select(
        outcome,
        timepoint,
        type,
        dose,
        class,
        condition,
        study,
        arm,
        intervention,
        mean,
        sd,
        se,
        n
      ) %>%
      mutate(arm = fct_relevel(arm, "placebo")) %>%
      arrange(study, arm) %>%
      group_by(study) %>%
      mutate(trt_ref = first(arm)) %>%
      ungroup()
  ),

  tar_target(
    smd_mood_cs_control,
    # identify placebo or first
    smd_mood_cs %>%
      filter(arm == trt_ref) %>%
      rename_with(~ glue("{.x}_control"),
                  c(mean, sd, se, n)) %>%
      # this is a follow-up timepiont, why is it labelled change score?!
      filter(!(
        str_detect(study, "creed 2003") & n_control == 72
      ))
  ),

  tar_target(
    smd_mood_cs_wide,
    smd_mood_cs %>%
      filter(arm != trt_ref) %>%
      left_join(
        smd_mood_cs_control  %>%
          select(study, mean_control, sd_control, se_control, n_control)
      ) %>%
      escalc(
        m1i = mean,
        m2i = mean_control,
        sd1i = sd,
        sd2i = sd_control,
        n1i = n,
        n2i = n_control,
        data = .,
        slab = study,
        measure = "SMD"
      ) %>%
      group_by(study)
  ),

  tar_target(
    smd_mood_cs_cor,

    cor(smd_mood_cs_wide$mean, smd_mood_cs_wide$mean_control)
  ),

  tar_target(
    smd_mood_cs_diff,
    smd_mood_cs_wide %>%
      bind_rows(smd_mood_cs_control %>% mutate(n = n_control)) %>%
      arrange(study) %>%
      mutate(
        diff = ifelse(is.na(mean),
                      NA,
                      mean),
        diff_se = ifelse(is.na(mean),
                         se_control,
                         sqrt(1 / (n + n_control) + diff ^ 2 / (2 * (
                           n + n_control
                         ))) * sqrt(2 * (1 - smd_mood_cs_cor)))
      )
  ),

  tar_target(
    smd_mood_cs_net,
    smd_mood_cs_diff %>%
      set_agd_contrast(
        study = study,
        trt = intervention,
        y = diff,
        se = diff_se,
        sample_size = n,
        trt_ref = "placebo"
      )
  ),

  tar_target(
    smd_mood_cs_nma,
    smd_mood_cs_net %>%
      nma(trt_effects = "random")
  ),




  # by outcome lor ----------------------------------------------------------

  # revman ------------------------------------------------------------------

  tar_target(
    revman_sof,
    oti_sof %>%
      left_join(intervention_key) %>%
      filter(outcome == "pain_mod") %>%
      arrange(desc(RCT)) %>%
      distinct() %>%
      select(-type,-outcome,-timepoint) %>%
      select(type = type_label, class, intervention, everything()) %>%
      # get ordering right
      mutate(class = fct_relevel(class, "snri", "tca")) %>%
      arrange(type, class, intervention, desc(RCT)) %>%
      # final wrangle
      select(-type) %>%
      mutate(
        class = if_else(
          class == "n/a",
          "Non-antidepressant",
          as.character(class) %>% toupper()
        ),
        intervention = str_to_sentence(intervention),
        Studies = glue("{RCT} ({studies})"),
        n = glue("{participants} ({n})")
      ) %>%
      select(-studies,-RCT,-participants) %>%
      # reorder columns
      select(class, intervention, nnt, rel, rank, n, high_risk_p, everything()) %>%
      rename(
        "Number of participants (proportion of outcome total)" = n,
        "Number of studies (proportion of outcome total)" = Studies,
        "Number needed to treat to benefit" = nnt,
        "Odds-ratio (95% credible interval)" = rel,
        "Rank estimates (95% credible interval)" = rank,
        "Certainity of evidence (GRADE)" = high_risk_p
      ) %>%
      rename_with(str_to_sentence,
                  everything()),
  ),

  tar_target(
    revman_total_n,
    mod_dat %>%
      filter(timepoint == "post_int") %>%
      group_by(outcome) %>%
      summarise(n = sum(n)) %>%
      filter(outcome %in% c("pain_int", "pain_sub")) %>%
      pull(n) %>% sum()
  ),



  # revman rob --------------------------------------------------------------

  tar_target(revman_rob_dat, {
    study_levels <-
      rob_output %>%
      ungroup() %>%
      arrange(desc(study)) %>%
      pull(study) %>% unique() %>% str_to_sentence()

    rob_output %>%
      pivot_longer(-c(study),
                   names_to = "category",
                   values_to = "category_value") %>%
      ungroup() %>%
      mutate(
        study = str_to_sentence(study),
        study = as_factor(study),
        study = fct_relevel(study, study_levels),
        category = if_else(
          category == "rob",
          " ",
          str_replace_all(category, "_", " ") %>% str_to_sentence()
        ),
        facets = if_else(category == " ",
                         "ROB",
                         "Risk of bias (ROB) criteria"),
        facets = fct_relevel(facets, "ROB", after = Inf),
        category_value = fct_relevel(category_value, "low", "unclear", "high"),
        category = fct_relevel(category,
                               "Sequence generation",
                               "Allocation concealment",
                               "Blinding of participants and personnel",
                               "Blinding of outcome assessors",
                               "Selective outcome reporting",
                               "Incomplete outcome data",
                               "Other sources of bias",
                               " ")

      ) %>%
      arrange(study)
  }),



  tar_target(revman_rob_grid, {
    plot <-
      revman_rob_dat %>%
      # remove after testing in viewer pane
      # head(60) %>%
      ggplot(aes(x = category, y = study, colour = category_value)) +
      geom_point(size = 1/2, alpha = 0.8) +
      theme_minimal(
        base_size = 4
      ) +
      theme(
        panel.border = element_rect(colour = "gray", fill = NA),
        panel.grid.major = element_line(linetype = "dotted", colour = "gray"),
        axis.text.x = element_text(angle = 90, hjust = 0),
        axis.title = element_blank(),
        legend.position = "top"
      ) +
      scale_color_discrete("",
                           type = c("#008000",
                                    "#ffcc00",
                                    # "#737373",
                                    "#800000")) +
      # place x axis at top
      scale_x_discrete(position = "top") +
      facet_grid(. ~ facets,
                 scales = "free",
                 space = "free")
    ggsave(
      "exports/rob-grid.png",
      plot,
      width = 6,
      height = 23,
      units = "cm"
    )
  }),

  tar_target(revman_rob_bar, {
    barplot <-
    revman_rob_dat %>%
      group_by(category) %>%
      ggplot(aes(x = category,
                 fill = category_value)) +
      geom_bar(
        # aes(y = (..count..) / sum(..count..)),
        alpha = 0.7


               ) +
      theme_minimal(base_size = 5) +
      # scale_y_continuous(labels = percent) +
      # place x axis at top
      # scale_y_discrete(position = "left") +
      coord_flip() +
      scale_fill_discrete("",
                          type = c("#008000",
                                   "#ffcc00",
                                   #"#737373",
                                   "#800000")) +
      theme(axis.title = element_blank(),
            panel.grid = element_blank())

    ggsave(
      "exports/rob-bar.png",
      barplot,
      width = 9,
      height = 4,
      units = "cm"
    )
  }),


  # revman outcome ----------------------------------------------------------
  tar_target(revman_outcome,
             "pain_sub"),

  tar_target(revman_timepoint,
             "post_int"),

  tar_target(revman_intervention,
             "esreboxetine"),

  tar_target(revman_subgroup,
             " "),

  tar_target(revman_subgroup_category,
             " "),


  tar_target(
    revman_all_int_dat,
    mod_dat %>%
      filter(
        # rob == revman_subgroup_category,
        # dose %in% c("n/a", revman_subgroup_category),
        # condition == revman_subgroup_category,
        outcome == revman_outcome,
        timepoint == revman_timepoint
      )

  ),

  tar_target(
    revman_dat,
    revman_all_int_dat %>%
      filter(intervention %in% c(revman_intervention, "placebo")
)
  ),

# inspect what combinations of interventions there are for studies
# that include intervention specified; i.e., identify other potential
# comparators
  tar_target(
    revman_combns, {
      these_studies <-
    revman_dat %>%
      filter(intervention != "placebo") %>%
      pull(study) %>% unique()

    this_int <- revman_intervention

    revman_all_int_dat %>%
      filter(study %in% these_studies) %>%
      group_by(study) %>%
      arrange(intervention) %>%
      summarise(
        ints = intervention %>% unique() %>% paste(collapse = '; ')
      ) %>%
      filter(
        ints != glue("{this_int}; placebo")
      )

    }
  ),


  tar_target(revman_pw_dat, {
    all_pw_dat <- revman_dat

    all_pw_dat %>%
      filter(intervention == "placebo") %>%
      select(study, r_placebo = r, n_placebo = n) %>%
      inner_join(all_pw_dat %>% filter(intervention != "placebo")) %>%
      distinct()

  }),

  tar_target(revman_pw_mod,
             {
               escalc(
                 ai = r,
                 ci = r_placebo,
                 n1i = n,
                 n2i = n_placebo,
                 data = revman_pw_dat,
                 measure = "OR",
                 slab = study
               ) %>%
                 rma(
                   yi = yi,
                   vi = vi,
                   # slab = study,
                   measure = "OR",
                   data  = .
                 )
             }),

  tar_target(revman_for, {
    mlabfun <- function(text, res) {
      list(bquote(
        paste(
          .(text),
          " (Q = ",
          .(formatC(
            res$QE, digits = 2, format = "f"
          )),
          ", df = ",
          .(res$k - res$p),
          ", p ",
          .(metafor:::.pval(
            res$QEp,
            digits = 2,
            showeq = TRUE,
            sep = " "
          )),
          "; ",
          I ^ 2,
          " = ",
          .(formatC(
            res$I2, digits = 1, format = "f"
          )),
          "%, ",
          tau ^ 2,
          " = ",
          .(formatC(
            res$tau2, digits = 2, format = "f"
          )),
          ")"
        )
      ))
    }


    to <- revman_outcome %>% str_remove("\\s\\(.+\\)")
    ti <- revman_intervention
    ts <- revman_subgroup
    tsc <- revman_subgroup_category

    this_title <-
      glue("{outcome_label(to)}: {ti}, {ts}:{tsc}")  %>%
      str_remove(",\\s+:\\s+") %>%
      str_to_sentence() %>%
      str_wrap()

    imgpath <-  glue("exports/revman/{to}/{ti}-{ts}-{tsc}-forest.png")
    dontpanic::msg(imgpath)
    png(imgpath)
    forest(
      revman_pw_mod,
      main = this_title,
      mlab = mlabfun(" ",
                     revman_pw_mod),
      atransf = exp
    )

    dev.off()
  }),

  tar_target(
    revman_regtest,
    regtest(revman_pw_mod)
  ),

  tar_target(
    revman_fun,{

    to <- revman_outcome %>% str_remove("\\s\\(.+\\)")
    ti <- revman_intervention
    ts <- revman_subgroup
    tsc <- revman_subgroup_category
    ci_lb <- round(revman_regtest$ci.lb,2)
    ci_ub <- round(revman_regtest$ci.ub,2)

    this_title <-
      glue("{outcome_label(to)} ({ci_lb}, {ci_ub}): {ti}, {ts}:{tsc}") %>%
      str_remove(",\\s+:\\s+") %>%
      str_to_sentence() %>%
      str_wrap()

    imgpath <-  glue("exports/revman/{to}/{ti}-{ts}-{tsc}-funnel.png")
    dontpanic::msg(imgpath)
    png(imgpath)
    funnel(
      revman_pw_mod,
      main = this_title,
      atransf = exp,
      level = c(90, 95, 99),
      # refline = 0,
      shade = c("white", "gray55", "gray75")
    )

    dev.off()}
  ),



  tar_target(
    revman_nma_mod,
    o_nma_key %>%
      filter(outcome == revman_outcome,
             timepoint == revman_timepoint) %>%
      pull(write_path) %>%
      read_rds() %>%
      pluck("result")
  ),

tar_target(
revman_pico,
revman_nma_mod %>%
  pluck("network", "agd_arm") %>%
  group_by(design) %>%
  summarise(
    studies = n_distinct(.study)
  )
  # pull(.study) %>% n_distinct()
  # group_by(study) %>%
  # count(condition)
  # pull(condition) %>% unique() %>% length() # paste(collapse = "; ")
),

tar_target(
  revman_ints_output,
  revman_nma_mod %>%
    pluck("network", "agd_arm") %>%
    group_by(.trt) %>%
    summarise(
      n = n_distinct(.study)
    ) %>%
    arrange(desc(n)) %>%
    mutate(
      ints = glue("{.trt} ({n})")
    ) %>%
    pull(ints) %>%
    paste(collapse = "; ")
),

tar_target(
  revman_conditions,
  revman_nma_mod %>%
    pluck("network", "agd_arm") %>%
    group_by(condition) %>%
    summarise(
      n = n_distinct(.study)
    ) %>%
    arrange(desc(n)) %>%
    mutate(
      con = glue("{condition} ({n})")
    ) %>%
    pull(con) %>%
    paste(collapse = "; ")
),

tar_target(
  revman_contrasts,
  revman_nma_mod %>%
    pluck("network", "agd_arm") %>%
    group_by(.study) %>%
    arrange(.trt) %>%
    summarise(
      ints = unique(.trt) %>% paste(collapse = ";")
    ) %>%
    filter(!str_detect(ints, "placebo;")) %>% arrange(ints)
),

  tar_target(
    revman_gradepro,
    oti_results %>%
      filter(
        outcome == revman_outcome,
        timepoint == revman_timepoint# ,
        # intervention == revman_intervention
      ) %>%
      ungroup() %>%
      mutate(n_int = n_total - n_placebo) %>%
      mutate(across(c(starts_with("nma_")), round, digits = 2)) %>%
      select(outcome, timepoint, intervention, studies_int, contains('nnt'), n_int,n_placebo, contains("nma"),  n_total) %>%
      gt()
  ),


  # null --------------------------------------------------------------------

  NULL
)
