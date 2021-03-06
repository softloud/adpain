# shorthand ---------------------------------------------------------------

# run specific parts of the pipeline: starts_with *_

# starts_with labels

# a := assert
# r := raw
# w := wrangling
# m := model
# c := check, for exporting checks and asserts
# e := exploratory data analysis table or vis
# p := pipeline design
# o: = write outputs
# w_cov: from covidence export
# w_obs: from hollie's exports

# secondary signifiers ^_*_

# h := prepared by hollie

list(
  # outcome labels ----------------------------------------------------------
  
  tar_target(
    r_outcome_labels,
    read_csv(
      "data/labels/outcome-2021-09-09 03:37:04.csv",
      col_types = cols(.default = "c")
    ) %>%
      clean_names()
  ),
  
  # covidence export --------------------------------------------------------
  
  tar_target(
    r_cov,
    #suppressWarnings(suppressMessages(
    read_csv("data/review_91309_extracted_data_csv_20210909121500.csv")
    #))
  ),
  
  tar_target(
    w_cov_cleaned,
    r_cov %>%
      clean_names() %>%
      select(
        study_cov = study_identifier,
        arm = intervention,
        title_cov = comments,
        everything()
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  # hollie's extractions ----------------------------------------------------
  
  tar_target(r_h_outcome_adverse,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Adverse Events.csv")
             ))),
  
  tar_target(r_h_outcome_mood,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Mood.csv"),
             ))),
  
  tar_target(r_h_outcome_pain_int,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Pain intensity.csv")
             ))),
  
  tar_target(r_h_outcome_pain_mod,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Moderate pain relief.csv")
             ))),
  
  tar_target(r_h_outcome_physical,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Physical function.csv")
             ))),
  
  tar_target(r_h_outcome_qol,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Quality of life.csv")
             ))),
  
  tar_target(r_h_outcome_sleep,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Sleep.csv")
             ))),
  
  tar_target(r_h_outcome_withdrawal,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Withdrawal.csv")
             ))),
  
  tar_target(r_h_outcome_pain_sub,
             suppressWarnings(suppressMessages(
               read_csv("data/outcomes-2021-09-09/Substantial pain relief.csv")
             ))),
  
  # put all observations in one list
  tar_target(
    r_h_outcome_obs,
    list(
      mood = r_h_outcome_mood,
      pain_int = r_h_outcome_pain_int,
      adverse = r_h_outcome_adverse,
      physical = r_h_outcome_physical,
      qol = r_h_outcome_qol,
      sleep = r_h_outcome_sleep,
      pain_mod = r_h_outcome_pain_mod,
      withdrawal = r_h_outcome_withdrawal,
      pain_sub = r_h_outcome_pain_sub
    )
    
  ),
  
  tar_target(w_outcomes,
             r_h_outcome_obs %>% names()),
  
  tar_target(
    w_outcome_obs,
    r_h_outcome_obs %>%
      pluck(w_outcomes) %>%
      # tidy up column names
      clean_names %>%
      # remove columns we'll pull from the study_arm df
      select(-any_of(
        c("intervention_name", "intervention_type")
      )) %>%
      rename(arm = intervention) %>%
      # add outcome
      mutate(outcome = w_outcomes) %>%
      select(
        outcome,
        study_obs = study_identifier,
        title_obs = comments,
        everything()
      ),
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  
  # take hollie's extractions long ------------------------------------------
  tar_target(
    w_obs_long,
    w_outcome_obs %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study_obs, arm, title_obs),
        names_to = "covidence_colname",
        values_to = "covidence_value",
        values_drop_na = TRUE
      ) %>%
      mutate(
        measure_matches =
          map(covidence_colname, str_match, "(.+)_([a-z]+)[_\\d]*$"),
        measure_type = map_chr(measure_matches, 3),
        covidence_desc = map_chr(measure_matches, 2)
      ) %>%
      select(-measure_matches,-covidence_colname) %>%
      mutate(across(everything(), tolower)) %>%
      left_join(m_key, by = "outcome")
    ,
    pattern = map(w_outcome_obs)
  ),
  
  
  
  # model types -------------------------------------------------------------
  
  tar_target(
    m_key,
    tibble(outcome = w_outcomes) %>%
      mutate(
        model_type = case_when(
          outcome == "adverse" ~ "lor",
          outcome == "pain_mod" ~ "lor",
          outcome == "pain_sub" ~ "lor",
          outcome == "withdrawal" ~ "lor",
          TRUE ~ "smd"
        ),
        model_text = if_else(model_type == "lor",
                             "odds ratio",
                             # don't forget to scale!
                             "standardised mean difference")
      ) %>%
      left_join(r_outcome_labels) %>%
      rename(outcome_direction = direction_of_improvement)
    
  ),
  
  # study labels ------------------------------------------------------------
  
  # create study key with study identifier, title, and unique study id
  
  tar_target(
    w_study_key,
    w_cov_cleaned %>%
      select(study_cov, title_cov) %>%
      distinct() %>%
      arrange(study_cov) %>%
      study_id() %>%
      mutate(
        title = str_replace(title_cov, "title: ", ""),
        study = as.character(study)
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  # now we have a study key we can instantiate study_arm info
  tar_target(
    w_cov_study,
    w_cov_cleaned %>%
      mutate(across(everything(), tolower)) %>%
      # apply study labels
      left_join(w_study_key,
                by = c("study_cov", "title_cov")) %>%
      select(study, everything(), study_cov, title_cov)
  ),
  
  tar_target(a_cov_study,
             {
               assert_that(nrow(w_cov_study) == nrow(r_cov),
                           msg = "Number of rows not equal to initial rows")
               
               
               count_unmatched <-
                 w_cov_study %>%
                 filter(is.na(study) | is.na(title)) %>%
                 nrow()
               
               assert_that(count_unmatched == 0,
                           msg = "NAs in either study or title label")
             }),
  
  # apply study labels to observations
  tar_target(
    w_obs_study,
    w_obs_long %>%
      left_join(
        w_study_key,
        by = c("study_obs" = "study_cov",
               "title_obs" = "title_cov")
      ) %>%
      select(outcome, study, arm, everything())
  ),
  
  
  # study unmatched ---------------------------------------------------------
  
  tar_target(
    w_study_unmatched,
    w_obs_study %>%
      filter(is.na(study)) %>%
      select(study_obs, title_obs) %>%
      distinct()
  ),
  
  tar_target(
    w_study_unmatched_studies,
    w_study_unmatched %>% pull(study_obs)
  ),
  
  tar_target(
    w_study_unmatched_key,
    w_study_key %>%
      filter(study_cov %in% w_study_unmatched_studies)
  ),
  
  # as the studis that are unmatched have only a single,
  # we can join on study only
  tar_target(
    w_obs_study_fix,
    w_obs_study %>%
      mutate(
        study = map2_chr(
          study,
          study_obs,
          .f = function(s, h) {
            if (is.na(s)) {
              w_study_unmatched_key %>%
                filter(study_cov == h) %>% pull(study)
            } else
              s
          }
        ),
        title = map2_chr(
          study_obs,
          title,
          .f = function(h, t) {
            if (is.na(t)) {
              w_study_unmatched_key %>%
                filter(study_cov == h) %>% pull(title)
            } else
              t
          }
        )
      )
    
  ),
  
  tar_target(a_study, {
    count_unmatched <-
      w_obs_study_fix %>%
      filter(is.na(study) | is.na(title)) %>%
      nrow()
    
    assert_that(count_unmatched == 0,
                msg = "NAs in either study or title label")
  }),
  
  
  # where obs have cov join labels correct ----------------------------------
  
  tar_target(w_obs_long_final,
             w_obs_study_fix),
  
  # output study matches
  tar_target(
    w_study_report,
    w_cov_study %>%
      select(study, title, study_cov, title_cov) %>%
      left_join(w_obs_study_fix %>%
                  select(study, study_obs, title_obs),
                by = "study") %>%
      arrange(desc(str_detect(study, ":")),
              desc(is.na(study_obs))) %>%
      select(contains("study"), everything()) %>%
      distinct()
  ),
  
  # go wide -----------------------------------------------------------------
  
  tar_target(
    w_obs_wide,
    w_obs_long_final %>%
      select(-study_obs, -title_obs) %>%
      pivot_wider(names_from = measure_type,
                  values_from = covidence_value)
  ),
  # timepoints --------------------------------------------------------------
  
  tar_target(
    w_obs_timepoint,
    w_obs_wide %>%
      dplyr::mutate(
        # specific study changes
        timepoint = dplyr::case_when(
          # change points have change in covidence
          str_detect(covidence_desc, "change") ~ "change_score",
          # see issue # 26 hpp
          str_detect(study, "pirbudak 2003") &
            str_detect(covidence_desc, "9_months") ~ 'post_int',
          str_detect(study, "pirbudak 2003") &
            str_detect(covidence_desc, "2_weeks|6_weeks|3_months|6_month") ~ 'mid_int',
          str_detect(study, "engel 1998") ~ "post_int",
          str_detect(study, "johansson") ~ "post_int",
          str_detect(study, "ginsberg") &
            str_detect(covidence_desc, "halfway") ~ "mid_int",
          str_detect(study, "grace 1985") &
            str_detect(covidence_desc, "week_[48]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(covidence_desc, "week_[123]") ~ "mid_int",
          str_detect(study, "leijon") &
            str_detect(covidence_desc, "week_4") ~ "post_int",
          outcome == "adverse" &
            str_detect(study, "agger") ~ "post_int",
          outcome == "pain" &
            str_detect(study, "bansal 2009") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(covidence_desc, "8_weeks|week_8") ~ "post_int",
          str_detect(study, "nct 2003") &
            str_detect(covidence_desc, "months") ~ "follow_up",
          
          # general labels
          str_detect(covidence_desc, "follow_up") ~ "follow_up",
          stringr::str_detect(
            covidence_desc,
            "end_of_treatment|post_treatment|post_intervention|endpoint|ednpoint"
          ) ~ "post_int",
          str_detect(covidence_desc, "mid_intervention|within_trial") ~ "mid_int",
          str_detect(covidence_desc, "baseline") ~ "baseline",
          TRUE ~ "unmatched"
        )
      ) %>%
      select(outcome, study, arm, covidence_desc, timepoint, everything())
  ),
  
  
  # tar_target(a_obs_timepoint, {
  #   timepoints <-
  #     w_obs_timepoint %>%
  #     pull(timepoint) %>% unique()
  #
  #   # check that timepoints match the level
  #   assert_that(all(timepoints != "unmatched"),
  #               msg = "unmatched timepoints")
  # }),
  
  # wrangle scales ----------------------------------------------------------
  
  tar_target(r_scales,
             read_rds("data/scales-2021-09-06_05:19:18.rds")),
  
  tar_target(
    w_scales_aka,
    r_scales %>%
      clean_names() %>%
      rename(scale_category = scale) %>%
      mutate(
        outcome_label = outcome,
        scale_label = scale_category,
        scale_category = tolower(scale_category),
        aka = tolower(aka),
        outcome = tolower(outcome),
        outcome =
          case_when(
            str_detect(outcome, "mood") ~ "mood",
            outcome == "pain" ~ "pain_int",
            str_detect(outcome, "physical") ~ "physical",
            str_detect(outcome, "quality") ~ "qol",
            TRUE ~ outcome
          )
      ) %>%
      mutate(aka = strsplit(aka, split = "\\s*;\\s*"))  %>%
      unnest(aka) %>%
      # clean the aka column so it's the same as the cleaned janitor names
      mutate(
        # store raw aka for troubleshooting
        aka_raw = aka,
        # remove whitespace from start and end
        aka = str_trim(aka),
        aka =
          # replace () and [] with nothing
          str_replace_all(aka, "[\\(\\)\\[\\]]", "") %>%
          # replace - and space with _
          str_replace_all("[\\s+-]", "_")
      ) %>%
      mutate(
        scale_category =
          str_replace_all(scale_category, "[\\s+\\-,]", "_") %>%
          str_remove_all("[\\(\\)]") %>%
          str_replace_all("_+", "_")
      )
    
  ),
  
  tar_target(
    w_scales,
    # add upper
    w_scales_aka %>%
      mutate(across(everything(), tolower)) %>%
      ungroup() %>%
      mutate(
        upper_range_num = map(upper_range, ~ unlist(.x) %>% as.numeric()),
        upper_range_num = as.numeric(upper_range_num)
      )
    
  ),
  
  # scale matches -----------------------------------------------------------
  
  tar_target(
    w_obs_scale_matches,
    w_obs_timepoint %>%
      mutate(scale_matches = pmap(
        list(outcome,
             covidence_desc,
             model_type),
        .f = scale_match,
        scale_df = w_scales
      ))
  ),
  
  
  
  tar_target(
    w_obs_scale_counts,
    w_obs_scale_matches %>%
      # select(study, outcome, covidence_desc, scale_match) %>%
      mutate(cat_n = map_int(
        scale_matches,
        .f = function(df) {
          df %>%
            select(scale_category) %>%
            distinct() %>%
            nrow()
        }
      ))
  ),
  
  tar_target(w_obs_scale_snapshot,
             w_obs_scale_counts %>%
               count(cat_n)),
  
  tar_target(w_obs_scale_excluded,
             w_obs_scale_counts %>%
               filter(cat_n != 1)),
  
  # patch in september labels
  tar_target(
    w_obs_scale_fix_dat,
    read_csv("data/labels/scale_unmatched-2021-09-09 04:57:48.csv") %>%
      select(hollie_scale, covidence_desc, outcome, study)
  ),
  
  
  tar_target(
    w_obs_scale_fix,
    w_obs_scale_excluded %>%
      left_join(w_obs_scale_fix_dat) %>%
      rename(scale = hollie_scale)
  ),
  
  
  tar_target(
    w_obs_scale_viable_matching,
    w_obs_scale_counts %>%
      filter(cat_n == 1) %>%
      mutate(scale = map_chr(
        scale_matches,
        .f = function(df) {
          df %>% select(scale_category) %>%
            distinct() %>% pull(scale_category)
        }
      )) %>%
      select(-scale_matches, -cat_n) %>%
      ungroup()
  ),
  
  tar_target(
    w_obs_scale_viable,
    bind_rows(w_obs_scale_fix, w_obs_scale_viable_matching)
  ),
  
  tar_target(
    w_obs_scale_ranked,
    w_obs_scale_viable %>%
      group_by(outcome, scale) %>%
      count(outcome, scale) %>%
      arrange(outcome, desc(n)) %>%
      group_by(outcome) %>%
      mutate(outcome_scale_rank = row_number()) %>%
      rename(number_of_scales_per_outcome = n)
  ),
  
  tar_target(
    w_obs_scale,
    w_obs_scale_viable %>%
      select(outcome, study, arm, scale, everything()) %>%
      ungroup() %>%
      left_join(
        w_scales %>% ungroup() %>% select(
          outcome,
          scale = scale_category,
          direction_of_improvement,
          upper_range,
          upper_range_num
        ) %>% distinct(),
        by = c("outcome", "scale")
      ) %>%
      left_join(w_obs_scale_ranked) %>%
      group_by(outcome, study, arm) %>%
      filter(outcome_scale_rank == min(outcome_scale_rank)) %>%
      ungroup() %>%
      distinct()
  ),
  
  # tar_target(a_scales,
  #            {
  #              # check number of studies in scale
  #              n_study_scale <-
  #                w_obs_scale %>%
  #                pull(study) %>%
  #                unique() %>% length()
  #
  #              n_study_key <-
  #                w_study_key %>%
  #                pull(study) %>%
  #                unique() %>% length()
  #
  #
  #              assert_that(n_study_scale == n_study_key,
  #                          message = "Different number of studies in w_obs_scale and w_study_key")
  #
  #            }),
  
  # calculations ------------------------------------------------------------
  
  tar_target(
    w_obs_numeric,
    w_obs_scale %>%
      mutate(across(any_of(
        c("mean", "sd", "se", "percent", "median")
      ), as.numeric)) %>%
      mutate(n = as.integer(n))
    
  ),
  
  # get sds
  tar_target(
    w_obs_calc,
    w_obs_numeric %>%
      mutate(
        # fix this later
        r = ifelse(!is.na(percent), n, NA),
        se = if_else(sd > 0 & is.na(se) & n > 0,
                     sd / sqrt(n),
                     se),
        sd = if_else(se > 0 & is.na(sd) & n > 0,
                     se * sqrt(n),
                     sd),
        n = if_else(model_type == "lor" & percent > 0,
                    as.integer(r * 100 / percent),
                    n)
      )
  ),
  
  
  tar_target(
    w_obs_scale_direction,
    w_obs_calc %>%
      select(-scale_matches, -cat_n) %>%
      # some of the scales haven't got direction of improvement
      select(-direction_of_improvement) %>%
      left_join(
        w_scales %>% select(scale = scale_category,
                            direction_of_improvement)
      ) %>%
      mutate(
        mean = if_else(
          is.character(direction_of_improvement) &
            direction_of_improvement != outcome_direction,
          -mean,
          mean
        )
      )
  ),
  
  tar_target(
    w_obs_direction_exclusions,
    w_obs_scale_direction %>%
      filter(model_type == "smd") %>%
      filter(
        is.na(direction_of_improvement) |
          direction_of_improvement == "unknown"
      )
  ),
  
  tar_target(
    w_obs_viable,
    w_obs_scale_direction %>%
      anti_join(w_obs_direction_exclusions) %>%
      mutate(
        model_viable = case_when(
          model_type == "lor" ~ n > 0 & r > 0,
          model_type == "smd" ~ mean > 0 &
            se > 0 & n > 0
        )
      ) %>%
      filter(model_viable) %>%
      filter(!is.na(study)) %>%
      select(-model_viable)
  ),
  
  tar_target(w_obs_investigate,
             anti_join(w_obs_calc, w_obs_viable)),
  
  
  # end observation wrangling -----------------------------------------------
  
  
  
  # wrangle condition dataset -----------------------------------------------
  
  tar_target(
    r_condition,
    read_csv("data/conditions-2021-07-12_16:37:56.csv")
  ),
  
  tar_target(
    w_condition,
    r_condition %>%
      select(-condition_no_chronic) %>%
      rename(
        condition_study = chronic_condition,
        condition_general = general_grouping,
        condition_iasp = iasp_classification
      )
  ),
  
  
  # wrangle covidence export pars, vars, groups -----------------------------
  
  # initial label changes, etc.
  tar_target(
    w_cov_initial,
    w_cov_study  %>%
      select(-design) %>% # RCT repeated
      select(
        study,
        arm,
        intervention_type,
        intervention_class,
        intervention_name,
        design = group,
        main_aim = main_aim_pain_mood_quality_of_life_etc,
        intervention = intervention_name,
        type = intervention_type,
        class = intervention_class,
        condition_study = chronic_pain_condition_s,
        title
      )  %>%
      mutate(
        # fix a typo in placebo
        type = if_else(type == "placeco",
                       "placebo",
                       type),
        
        # fix a spelling mistake in classes
        class = if_else(str_detect(class, "tetra"),
                        "tetracyclic (teca)",
                        class),
        
        # one of the placebo classes is labelled "n"
        class = ifelse(class == "n", NA, class),
        # relabel control as placebo
        intervention = if_else(intervention == "control", "placebo", intervention),
        intervention = if_else(arm == "placebo" &
                                 is.na(intervention), "placebo", intervention)
        
      )
  ),
  
  tar_target(a_cov_label, {
    assert_that(nrow(w_cov_initial) == nrow(r_cov))
  }),
  
  # type --------------------------------------------------------------------
  
  tar_target(
    w_cov_type_grouping,
    read_csv("data/labels/type-2021-09-09 10:01:24.csv") %>%
      select(type = intervention_type, intervention_grouping) %>%
      distinct()
  ),
  
  tar_target(
    w_cov_type,
    w_cov_initial %>%
      left_join(w_cov_type_grouping,
                by = "type") %>%
      rename(subtype = type,
             type = intervention_grouping)
  ),
  
  tar_target(a_cov_type,
             
             assert_that(nrow(w_cov_type) == nrow(r_cov))),
  
  
  # interventions -----------------------------------------------------------
  
  
  # clean all interventions
  tar_target(
    w_cov_int,
    w_cov_type %>%
      mutate(
        # label placebo interventions
        intervention = if_else(is.na(intervention) &
                                 type == "placebo",
                               "placebo",
                               intervention),
        # fix 3 missing labels
        intervention = case_when(
          intervention == "n" & type == "placebo" ~ "placebo",
          intervention == "n" &
            type == "antidepressant" &
            str_detect(arm, "mirtazapine") ~ "mirtazapine",
          intervention == "n" & arm == "placebo and cbt" ~
            "placebo + cbt",
          is.na(intervention) &
            type == "antidepressant" &
            str_detect(arm, "venlafaxine") ~ "venlafaxine",
          TRUE ~ intervention
        ),
        # assumptions checked with hollie
        # documented in #51
        intervention = case_when(
          intervention == "desipramine hydrochloride" ~ "desipramine",
          str_detect(intervention, "^duloxetine") ~ "duloxetine",
          intervention == "venlafaxine xr" ~ "venlafaxine",
          str_detect(intervention, "^paroxetine") ~ "paroxetine",
          TRUE ~ intervention
        )
        
      )
  ),
  
  tar_target(
    w_cov_int_nonad_labels,
    read_csv("data/labels/intervention-nonad-2021-09-11 09:01:03.csv")
  ),
  
  tar_target(
    w_cov_int_nonad_label,
    w_cov_int %>%
      left_join(w_cov_int_nonad_labels,
                by = c('study', 'arm')) %>%
      mutate(intervention.x = if_else(
        is.na(intervention.x),
        intervention.y,
        intervention.x
      )) %>%
      select(intervention = intervention.x,
             everything()) %>%
      select(-intervention.y)
  ),
  
  
  tar_target(a_cov_int, {
    df <- w_cov_int_nonad_label %>%
      filter(is.na(intervention) |
               intervention == "n") %>%
      select(intervention, arm, type)
    
    assert_that(nrow(df) == 0,
                msg = "NAs or n in intervention column")
    
    assert_that(nrow(w_cov_int_nonad_label) == nrow(r_cov),
                msg = "Different number of rows to raw cov import")
  }),
  
  
  # class labels ------------------------------------------------------------
  tar_target(
    r_class,
    read_csv(
      "data/labels/class-2021-09-10 23:53:50.csv",
      col_types = cols(.default = "c")
    ) %>%
      clean_names() %>%
      mutate(across(everything(), tolower))
  ),
  
  tar_target(
    w_cov_class,
    w_cov_int_nonad_label %>%
      mutate(class = str_trim(class)) %>%
      left_join(r_class,
                by = c("class" = "original")) %>%
      mutate(class = case_when(
        !is.na(hollie) ~ hollie, !is.na(charles) ~ charles,
        TRUE ~ class
      )) %>%
      select(-charles, -hollie) %>%
      rename(subclass = class,
             class = protocol)
  ),
  
  
  
  # condition ---------------------------------------------------------------
  
  tar_target(w_cov_condition_fix, {
    read_csv("data/labels/condition-2021-09-09 04:57:48.csv",
             col_types = cols(.default = "c"))
    
  }),
  
  
  tar_target(w_cov_condition, {
    w_cov_type %>%
      left_join(w_condition %>% distinct(), by = "condition_study")
    
  }),
  
  tar_target(
    a_cov_condition,
    assert_that(nrow(w_cov_condition) == nrow(r_cov),
                msg = "not same number of rows as raw import of cov")
  ),
  
  # covidence final output --------------------------------------------------
  
  # this target always represents the final cleaned parameters, variables,
  # subgroups, etc. from covidence export
  tar_target(
    w_cov,
    w_cov_condition %>%
      mutate(condition_general =
               map2_chr(condition_general, study, function(c, s) {
                 if (!is.na(c))
                   c
                 else
                   w_cov_condition_fix %>%
                   filter(study == s) %>%
                   pull(condition_general)
               }))
  ),
  
  tar_target(a_cov, {
    assert_that(nrow(r_cov) == nrow(w_cov),
                msg = "w_cov does not have same number of rows as
                r_cov")
    
    
    assert_that(nrow(w_cov %>% filter(is.na(
      condition_general
    ))) == 0,
    msg = "Unmatched condition_general")
  }),
  
  
  # bundle everything together ----------------------------------------------
  
  # todo: add filters to observation
  
  # keep this target as the final all-in output dataframe to be shared
  # use this target for EDA
  tar_target(
    w_obs,
    w_obs_viable %>%
      left_join(w_cov, by = c("study", "arm")) %>%
      rename(covidence = covidence_desc) %>%
      select(study, intervention, arm, everything(), covidence) %>%
      relocate(r, .before = n) %>%
      ungroup() %>%
      # now select only what's needed
      select(
        outcome,
        model_type,
        study,
        intervention,
        arm,
        covidence,
        condition_general,
        condition_iasp,
        class,
        type,
        timepoint,
        design,
        mean,
        r,
        n,
        se,
        sd,
        scale,
        title = title.x
      ) %>%
      distinct(),
    pattern = NULL
  ),
  
  
  # create protocol labelling dataset ---------------------------------------
  
  tar_target(
    w_obs_protocol,
    w_obs %>%
      mutate(
        # final fixes
        
        type_protocol =
          case_when(
            str_detect(
              type,
              "placebo|antidepressant|non-pharmacological intervention|pharmacological intervention"
            ) ~ type,
            TRUE ~ "other"
          ),
        type_m = case_when(
          type == "placebo" ~ type,
          type == "antidepressant" ~ type,
          type == "non-pharmacological intervention" ~ "non_pharma",
          type == "pharmacological intervention" ~ "non_ad_pharma",
          type == "other" ~ type,
          str_detect(intervention, "\\+|and") ~ "combined",
          intervention == "placebo" ~ "placebo",
          is.na(type) ~ "unclassified"
        ),
        intervention = if_else(
          intervention == "n" & arm == "mirtazapine 30mg",
          "mirtazapine",
          intervention
        ),
        intervention = if_else(
          str_detect(intervention, "^dulox"),
          "duloxetine",
          intervention
        ),
        intervention = if_else(intervention == "n" &
                                 arm == "placebo",
                               "placebo",
                               intervention)
        
      ) %>%
      mutate(type_cov = type,
             type = type_m)
  ),
  
  
  # data that goes into the models ------------------------------------------
  
  
  tar_target(
    w_obs_m,
    w_obs_protocol %>%
      mutate(timepoint = fct_rev(timepoint)) %>%
      # filter these studies because hollie indicated in sheet
      filter(
        timepoint != "baseline",
        !str_detect(scale, "DELETE"),!is.na(intervention),
        !str_detect(covidence, "change"),
        type != "unclassified"
      ) %>%
      # filters for computational efficiency
      filter(
        timepoint == "post_int",
        type %in% c("antidepressant", "placebo")
      ) %>%
      # there are arms in same study labeled by same scale
      # for now will choose the first
      group_by(outcome, study, arm, scale, timepoint) %>%
      filter(covidence == first(covidence)) %>%
      ungroup() %>%
      # select only the things we need
      select(
        outcome,
        study,
        intervention,
        arm,
        mean,
        r,
        sd,
        se,
        n,
        covidence,
        type,
        timepoint,
        class,
        condition_general,
        condition_iasp,
        scale,
        model_type
      ) %>%
      distinct() %>%
      # this is for testing
      filter(
        outcome %in%
          c("pain_sub",
            "pain_mod",
            "mood",
            "sleep",
            "adverse")
      )
    
  ),
  
  tar_target(a_obs_m, {
    # check means
    assert_that(all(w_obs_m$mean > 0 |
                      is.na(w_obs_m$mean)),
                msg = "Not all means positive or NA")
    
    # check r
    assert_that(all(w_obs_m$r > 0 |
                      is.na(w_obs_m$r)),
                msg = "Not all r positive or NA")
    
    # check se
    assert_that(all(w_obs_m$se > 0 |
                      is.na(w_obs_m$se)),
                msg = "Not all se positive or NA")
    
  }),
  
  
  
NULL
)