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
  
  conflicted::conflict_prefer("filter", "dplyr")
  
})

installed_pkg <- installed.packages()[, 1]

if ("dontpanic" %in% installed_pkg) {
  require(dontpanic)
}


# functions ---------------------------------------------------------------
c(
  "study_id",
  "lotr_study_hash",
  "hpp_net",
  "hpp_themes",
  "viable_observations",
  "scale_match",
  "hpp_forest",
  "forest_multinma"
) %>%
  paste0("R/", ., ".R") %>%
  map(source)

safe_nma <- safely(nma, otherwise = "failed")

# shorthand ---------------------------------------------------------------

# run specific parts of the pipeline: starts_with *_

# starts_with labels

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


# set up targets ----------------------------------------------------------

tar_option_set(packages = "dplyr")
options(mc.cores = parallel::detectCores() - 1)


# buffer limit reached ----------------------------------------------------

# run this
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)


# begin targets -----------------------------------------------------------

list(
  # outcome labels ----------------------------------------------------------
  
  tar_target(
    w_outcome_labels,
    read_csv(
      "data/labels/outcome-2021-09-09 03:37:04.csv",
      col_types = cols(.default = "c")
    ) %>%
      clean_names()
  ),
  
  # class labels ------------------------------------------------------------
  tar_target(
    w_class,
    read_csv(
      "data/labels/class-2021-09-06 04:08:54.csv",
      col_types = cols(.default = "c")
    ) %>%
      clean_names()
  ),
  
  tar_target(
    w_cov_class,
    w_cov_intervention %>%
      left_join(w_class,
                by = c("class" = "original")) %>%
      mutate(class = case_when(
        !is.na(hollie) ~ hollie, !is.na(charles) ~ charles,
        TRUE ~ class
      )) %>%
      select(-charles, -hollie)
  ),
  
  
  # covidence export --------------------------------------------------------
  
  tar_target(
    r_covidence,
    #suppressWarnings(suppressMessages(
    read_csv("data/review_91309_extracted_data_csv_20210909121500.csv")
    #))
  ),
  
  tar_target(
    w_covidence_cleaned,
    r_covidence %>%
      clean_names() %>%
      select(
        study_covidence = study_identifier,
        arm = intervention,
        title_covidence = comments,
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
  
  # bring in the new stuff
  # the 2021-07-07 h_ exports have the same number of rows
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
      select(outcome,
             study_h = study_identifier,
             title_h = comments,
             everything()),
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  
  # take hollie's extractions long ------------------------------------------
  tar_target(
    w_obs_long,
    w_outcome_obs %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(
        cols = -c(outcome, study_h, arm, title_h),
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
      left_join(w_outcome_labels)
    
  ),
  
  # study labels ------------------------------------------------------------
  
  # create study key with study identifier, title, and unique study id
  
  tar_target(
    w_study_key,
    w_covidence_cleaned %>%
      select(study_covidence, title_covidence) %>%
      distinct() %>%
      arrange(study_covidence) %>%
      study_id() %>%
      mutate(
        title = str_replace(title_covidence, "Title: ", ""),
        study = as.character(study)
      ) %>%
      mutate(across(everything(), tolower))
  ),
  
  # now we have a study key we can instantiate study_arm info
  tar_target(
    w_covidence,
    w_covidence_cleaned %>%
      mutate(across(everything(), tolower)) %>%
      # apply study labels
      left_join(w_study_key, by = c("study_covidence", "title_covidence")) %>%
      select(study, everything(), study_covidence, title_covidence)
  ),
  
  tar_target(w_covidence_assert_study,
             {
               count_unmatched <-
                 w_covidence %>%
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
        by = c("study_h" = "study_covidence",
               "title_h" = "title_covidence")
      ) %>%
      select(outcome, study, arm, everything())
  ),
  # study unmatched ---------------------------------------------------------
  
  tar_target(
    w_study_unmatched,
    w_obs_study %>%
      filter(is.na(study)) %>%
      select(study_h, title_h) %>%
      distinct()
  ),
  
  tar_target(
    w_study_unmatched_studies,
    w_study_unmatched %>% pull(study_h)
  ),
  
  tar_target(
    w_study_unmatched_key,
    w_study_key %>%
      filter(study_covidence %in% w_study_unmatched_studies)
  ),
  
  # as the studis that are unmatched have only a single, we can join on study only
  tar_target(
    w_obs_study_fix,
    w_obs_study %>%
      mutate(
        study = map2_chr(
          study,
          study_h,
          .f = function(s, h) {
            if (is.na(s)) {
              w_study_unmatched_key %>%
                filter(study_covidence == h) %>% pull(study)
            } else
              s
          }
        ),
        title = map2_chr(
          study_h,
          title,
          .f = function(h, t) {
            if (is.na(t)) {
              w_study_unmatched_key %>%
                filter(study_covidence == h) %>% pull(title)
            } else
              t
          }
        )
      )
    
  ),
  
  tar_target(w_study_assert, {
    count_unmatched <-
      w_obs_study_fix %>%
      filter(is.na(study) | is.na(title)) %>%
      nrow()
    
    assert_that(count_unmatched == 0,
                msg = "NAs in either study or title label")
  }),
  

# where obs have cov join labels correct ----------------------------------

tar_target(
  w_obs_long_final,
  w_obs_study_fix
),  
  
  # go wide -----------------------------------------------------------------
  
  tar_target(
    w_obs_wide,
    w_obs_long_final %>%
      select(-study_h, -title_h) %>%
      pivot_wider(names_from = measure_type,
                  values_from = covidence_value)
  ),
  # timepoints --------------------------------------------------------------
  
  tar_target(
    w_obs_time,
    w_obs_wide %>%
      dplyr::mutate(
        # specific study changes
        timepoint = dplyr::case_when(
          # see issue # 26
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
      ungroup() %>%
      mutate(
        upper_range_num = map(upper_range, ~ unlist(.x) %>% as.numeric()),
        upper_range_num = as.numeric(upper_range_num)
      )
    
  ),
  
  # scale matches -----------------------------------------------------------
  
  tar_target(
    w_obs_scale_matches,
    w_obs_time %>%
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
  
  tar_target(
    w_obs_scales_viable,
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
    w_obs_scale_ranked,
    w_obs_scales_viable %>%
      group_by(outcome, scale) %>%
      count(outcome, scale) %>%
      arrange(outcome, desc(n)) %>%
      group_by(outcome) %>%
      mutate(outcome_scale_rank = row_number()) %>%
      mutate(number_of_scales_per_outcome = n)
  ),
  
  tar_target(
    w_obs_scales,
    w_obs_scales_viable %>%
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
      ungroup()
  ),
  
  # # dose --------------------------------------------------------------------
  #
  #
  # tar_target(
  #   w_par_dose,
  #   w_par_labels %>%
  #     mutate(dose_extract = map(arm, str_extract_all, "\\d+\\w*")) %>%
  #     select(dose_extract, everything())
  #
  # ),
  #
  #
  #
  #
  
  # calculations ------------------------------------------------------------
  
  tar_target(
    w_obs_numeric,
    w_obs_scales %>%
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
      mutate(
        mean = if_else(
          direction_of_improvement == "Higher" &
            !is.na(upper_range_num),
          upper_range_num - mean,
          mean
        ),
        direction_of_improvement = if_else(
          direction_of_improvement == "Higher" &
            !is.na(upper_range_num),
          "Lower",
          direction_of_improvement
        )
      )
  ),
  
  tar_target(
    w_obs_viable,
    w_obs_scale_direction %>%
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
    w_cov_label,
    w_covidence  %>%
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
      left_join(w_condition, by = "condition_study") %>%
      mutate(
        # classes
        
        # fix a spelling mistake in classes
        class = if_else(str_detect(class, "tetra"),
                        "tetracyclic (teca)",
                        class),
        
        # one of the placebo classes is labelled "n"
        class = ifelse(class == "n", NA, class)
      )
  ),
  
  # type
  tar_target(w_cov_typo,
             w_cov_label %>%
               mutate(# fix a typo
                 type = if_else(
                   type == "placeco",
                   "placebo",
                   type
                 ))),
  
  # clean all interventions
  tar_target(
    w_cov_intervention,
    w_cov_typo %>%
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
          is.na(intervention) &
            type == "antidepressant" &
            str_detect(arm, "venlafaxine") ~ "venlafaxine",
          TRUE ~ intervention
        ),
        # assumptions to be checked with hollie
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
  
  tar_target(w_cov_int_assert, {
    df <- w_cov_intervention %>%
      filter(is.na(intervention) |
               intervention == "n") %>%
      select(intervention, arm, type)
    
    assert_that(nrow(df) == 0,
                msg = "NAs or n in intervention column for placebo|antidepressant type")
  }),
  
  
  # covidence final output --------------------------------------------------
  
  # this target always represents the final cleaned parameters, variables,
  # subgroups, etc. from covidence export
  tar_target(w_cov,
             w_cov_class),
  
  
  # find out what study arms don't match
  tar_target(
    w_cov_befaft,
    w_covidence %>%
      select(study, arm)
    %>%
      left_join(w_cov %>% select(study, arm))
  ),
  
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
        class,
        type,
        timepoint,
        design,
        mean,
        r,
        n,
        se,
        sd
      ) %>%
      distinct()
  ),
  
  tar_target(w_obs_m,
             w_obs  %>%
               # apply filters here for now
               filter(
                 timepoint == "post_int",
                 # this is a sledgehammer
                 type %in% c("placebo", "antidepressant")
               )),
  
  tar_target(w_obs_m_assert, {
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
  
  
  
  # excluded study-arms -----------------------------------------------------
  
  tar_target(excluded_studies, {
    before <- w_covidence %>%
      select()
  }),
  
  # models ------------------------------------------------------------------
  
  tar_target(
    m_all_in,
    {
      dat <-
        w_obs_m %>%
        filter(outcome == w_outcomes) %>%
        hppapp::viable_observations()
      m_type <-
        m_key %>%
        filter(outcome == w_outcomes) %>%
        pull(model_type)
      
      hpp_net(dat, m_type) %>%
        nma(trt_effects = "random")
    },
    pattern = map(w_outcomes),
    iteration = "list"
  ),
  
  
  # models by condition -----------------------------------------------------
  
  tar_target(
    m_condition_group,
    w_obs_m %>%
      group_by(outcome, condition_general) %>%
      tar_group(),
    iteration = "group"
    
  ),
  
  tar_target(
    m_condition,
    {
      dat <-
        m_condition_group %>%
        hppapp::viable_observations()
      
      if (nrow(dat) == 0) {
        return("no obs")
      } else if ((dat %>% filter(type == "placebo") %>% nrow) == 0) {
        return("no placebo")
      } else if ((dat %>% pull(study) %>% unique() %>% length()) == 1) {
        "only one study"
      }  else {
        m_type <-
          m_condition_group %>%
          pull(model_type) %>% unique()
        
        # return(m_type)
        
        hpp_net(dat, m_type) %>%
          nma(trt_effects = "random")
      }
    },
    pattern = map(m_condition_group),
    iteration = "list"
  ),
  
  
  # bundle models -----------------------------------------------------------
  
  
  tar_target(m_models,
             c(m_all_in, m_condition)),
  
  tar_target(m_model_key_w_fails,
             {
               m_models %>%
                 map_df(
                   .f = function(m_models_this) {
                     if (!is.list(m_models_this))
                       return(tibble(outcome = NA,
                                     condition = NA))
                     
                     this_dat <-
                       m_models_this$network$agd_arm
                     
                     # get outcome
                     this_outcome <-
                       this_dat$outcome %>% unique()
                     
                     # get conditions
                     conditions <-
                       this_dat %>%
                       pull(condition_general) %>%
                       unique()
                     this_condition <-
                       if (length(conditions) == 1)
                         conditions
                     else
                       "all"
                     
                     
                     # output df
                     tibble(outcome = this_outcome,
                            condition = this_condition)
                     
                   }
                 ) %>% mutate(model_index = row_number())
             }),
  
  tar_target(
    m_model_key,
    m_model_key_w_fails %>%
      filter(!is.na(outcome)) %>%
      mutate(plot_index = row_number()) %>%
      left_join(m_key, by = "outcome")
  ),
  
  
  # plots -------------------------------------------------------------------
  
  tar_target(
    plot_write_key,
    
    # plot_write_key <-
    m_model_key %>%
      mutate(
        filename = glue("{outcome}-{condition}"),
        netpath = glue("images/net/{filename}.png"),
        forestpath = glue("images/forest/{filename}.png")
      )
  ),
  
  
  tar_target(plot_net, {
    m_model_key %>%
      pmap(function(outcome, condition, model_index, ...) {
        m_models[[model_index]] %>%
          pluck("network") %>%
          plot() +
          labs(
            title = glue("Direct evidence for {outcome}"),
            subtitle = glue("Condition: {condition}")
          )
      })
  }),
  
  tar_target(
    plot_net_write,
    plot_write_key %>%
      select(plot_index, netpath) %>%
      pmap(
        .f = function(plot_index,  netpath) {
          ggsave(here::here("bksite", netpath), m_net_plot[[plot_index]])
        }
      )
  ),
  
  # this is a check that forest_multinma works
  tar_target(plot_forest_generic, {
    print("--] Select an arbitrary model")
    
    this_mod <-
      m_model_key %>%
      head(1) %>%
      pull(model_index) %>%
      m_models[[.]]
    
    # print(summary(this_mod))
    
    print("--] Identify the dataframe req for conf ints text")
    # this_mod
    
    print("--] Plot generic forest")
    
    forest_multinma(this_mod)
  }),
  
  tar_target(plot_forest_dev, {
    # select an arbitrary lor model
    m_model_key_row <-
      m_model_key %>%
      filter(model_type == "smd",!is.na(condition)) %>%
      tail(1)
    
    print("--] Selected row from m_model_key")
    print(m_model_key_row)
    
    print("--] Create plot")
    hpp_forest(m_models[[m_model_key_row$model_index]],
               m_model_key,
               m_model_key_row$model_index)
    
  }),
  
  tar_target(plot_forest, {
    # plot_forest <-
    m_model_key %>%
      select(model_index) %>%
      pmap(function(model_index) {
        hpp_forest(m_models[[model_index]], m_model_key, model_index)
      })
  }),
  
  tar_target(
    plot_forest_write,
    plot_write_key %>%
      select(plot_index, forestpath) %>%
      pmap(
        .f = function(plot_index,  forestpath) {
          ggsave(
            here::here("bksite", forestpath),
            plot_forest[[plot_index]],
            width = 11,
            height = 11
          )
        }
      )
  ),
  
  
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
  # pairwise ----------------------------------------------------------------
  
  # code single pairwise meta-analyses
  tar_target(pw_single_lor,
             {
               # select an arbitrary lor model
               m_model_key_row <-
                 m_model_key %>%
                 filter(model_type == "lor",!is.na(condition)) %>%
                 tail(1)
               
               print("--] Selected row from m_model_key")
               print(m_model_key_row)
               
               # get input data for that model
               mod_dat <-
                 m_models[[m_model_key_row$model_index]] %>%
                 pluck("network", "agd_arm") %>%
                 # rename to get back to original labels
                 rename(study = .study,
                        intervention = .trt)
               
               print("--] Check outcome and condition match.")
               
               mod_dat %>%
                 select(outcome, condition_general) %>%
                 distinct() %>%
                 print()
               
               print("--] Count interventions")
               
               mod_dat %>%
                 count(intervention) %>%
                 print()
               
               print("--] Yay! There is only one intervention :)")
               
               # intervention-level dataset
               
               print("--] Set intervention-level dataset")
               
               int_dat <-
                 mod_dat %>%
                 # select just the cols needed for pw
                 select(study, intervention, model_type, r, n)
               
               print("--] Columns in intervention-level dataset:")
               
               int_dat %>%
                 names() %>%
                 print()
               
               print("--] Look at intervention-level dataset")
               
               int_dat %>%
                 print()
               
               print("--] Go wide for metafor")
               
               int_dat_wide <-
                 int_dat %>%
                 pivot_wider(
                   names_from = intervention,
                   names_glue = "{.value}_{intervention}",
                   values_from = c(r, n)
                 )
               
               int_dat_wide %>%
                 names() %>% 
                 print()
               
               print("--] escalc")
               
               int_escalc <-
                 int_dat_wide %>%
                 escalc(data = .,
                        ai = r_amitriptyline,
                        ci = r_placebo,
                        n1i = n_amitriptyline,
                        n2i = n_placebo,
                        measure = "OR",
                        slab = study
                        )
               
               int_escalc %>% print()
               
               print("--] Meta-analyse!")
               
               int_escalc %>% 
                 rma(yi, vi, data = ., measure = "OR")
               
             }),
  
  tar_target(pw_single_smd,
             NULL),
  
  
  # null --------------------------------------------------------------------
  
  NULL
)
