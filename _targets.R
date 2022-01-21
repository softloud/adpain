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
  library(conflicted)

  conflicted::conflict_prefer("filter", "dplyr")
  conflict_prefer("discard", "scales")
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
      ) %>%
      mutate(class = if_else(
        intervention == "venlafaxine", "snri", class
      ))

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


  # rob vis --------------------------------------------------------------

  tar_target(rob_dat, {
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
        category = fct_relevel(
          category,
          "Sequence generation",
          "Allocation concealment",
          "Blinding of participants and personnel",
          "Blinding of outcome assessors",
          "Selective outcome reporting",
          "Incomplete outcome data",
          "Other sources of bias",
          " "
        )

      ) %>%
      arrange(study)
  }),



  tar_target(rob_grid, {
    plot <-
      rob_dat %>%
      # remove after testing in viewer pane
      # head(60) %>%
      ggplot(aes(x = category, y = study, colour = category_value)) +
      geom_point(size = 1 / 2, alpha = 0.8) +
      theme_minimal(base_size = 4) +
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
      "report/img/rob-grid.png",
      plot,
      width = 6,
      height = 23,
      units = "cm"
    )
  }),

  tar_target(rob_bar, {
    barplot <-
      rob_dat %>%
      group_by(category) %>%
      mutate(category_value = fct_rev(category_value)) %>%
      ggplot(aes(x = category,
                 fill = category_value)) +
      geom_bar(# aes(y = (..count..) / sum(..count..)),
        alpha = 0.7) +
      theme_minimal(base_size = 5) +
      scale_y_continuous(position = "right") +
      # place x axis at top
      # scale_y_discrete(position = "left") +
      coord_flip() +
      scale_fill_discrete("",
                          type = c("#800000",
                                   "#ffcc00",
                                   #"#737373",
                                   "#008000")) +
      theme(axis.title = element_blank(),
            panel.grid = element_blank()) +
      facet_grid(facets ~ .,
                 scales = "free",
                 space = "free")

    ggsave(
      "report/img/rob-bar.png",
      barplot,
      width = 9,
      height = 4,
      units = "cm"
    )
  }),


  # dich outcome ----------------------------------------------------------
  tar_target(dich_outcome,
             # "pain_mod"
             # "pain_mod"
             "pain_mod"),


  tar_target(dich_intervention,
             "milnacipran"),

  tar_target(dich_comparator,
             "placebo"),

  tar_target(dich_subgroup,
             "main_aim"),

  tar_target(dich_timepoint,
             "post_int"),

  # dich dat ----------------------------------------------------------------



  tar_target(
    dich_all_dat,
    mod_dat %>%
      filter(outcome == dich_outcome,
             timepoint == dich_timepoint) %>%
      mutate(class = toupper(class))
  ),

  tar_target(dich_all_int_dat,
             dich_all_dat),

  tar_target(dich_dat,
             dich_all_int_dat %>%
               filter(
                 intervention %in% c(dich_intervention, dich_comparator)
               )),

  # inspect what combinations of interventions there are for studies
  # that include intervention specified; i.e., identify other potential
  # comparators
  tar_target(dich_combns, {
    these_studies <-
      dich_dat %>%
      filter(intervention != "placebo") %>%
      pull(study) %>% unique()

    this_int <- dich_intervention

    dich_all_int_dat %>%
      filter(study %in% these_studies) %>%
      group_by(study) %>%
      arrange(intervention) %>%
      summarise(ints = intervention %>% unique() %>% paste(collapse = '; ')) %>%
      filter(ints != glue("{this_int}; placebo"))

  }),


  # dich meta-analysis -----------------------------------------------------------


  tar_target(dich_pw_dat, {
    dich_dat %>%
      filter(intervention == dich_comparator) %>%
      select(study, r_control = r, n_control = n) %>%
      inner_join(dich_dat %>% filter(intervention != dich_comparator)) %>%
      distinct()

  }),

  tar_target(
    dich_pw_escalc,
    escalc(
      ai = r,
      ci = r_control,
      n1i = n,
      n2i = n_control,
      data = dich_pw_dat,
      measure = "OR",
      slab = study
    )
  ),

  tar_target(dich_pw_mod,
             {
               dich_pw_escalc %>%
                 rma(
                   yi = yi,
                   vi = vi,
                   slab = study,
                   measure = "OR",
                   data  = .
                 )
             }),

  tar_target(mlabfun,
             function(text, res) {
               list(bquote(
                 paste(
                   .(text),
                   " Q = ",
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
                   ""
                 )
               ))
             }),

  tar_target(
    dich_rma_prefix,
    sprintf(
      "report/img/%s-%s-%s-%s",
      dich_outcome,
      dich_subgroup,
      dich_intervention,
      dich_comparator
    ) %>%
      str_remove("\\s-")
  ),

  tar_target(dich_for, {
    this_title <-
      sprintf("%s: %s vs %s",
              outcome_label(dich_outcome),
              dich_intervention,
              dich_comparator) %>%
      str_remove(",\\s+:\\s+") %>%
      str_to_sentence() %>%
      str_wrap()

    imgpath <-
      sprintf("%s-forest.png", dich_rma_prefix)

    dontpanic::msg(imgpath)
    png(imgpath)
    forest(
      dich_pw_mod,
      main = this_title,
      mlab = mlabfun("",
                     dich_pw_mod),
      atransf = exp
    )

    dev.off()
  }),

  tar_target(dich_regtest,
             regtest(dich_pw_mod)),

  tar_target(dich_fun, {
    to <- dich_outcome %>% str_remove("\\s\\(.+\\)")
    ti <- dich_intervention
    ts <- dich_subgroup
    ci_lb <- round(dich_regtest$ci.lb, 2)
    ci_ub <- round(dich_regtest$ci.ub, 2)

    this_title <-
      glue("{ti}, {ts}") %>%
      str_remove(",\\s+") %>%
      str_to_sentence() %>%
      str_wrap()

    imgpath <-
      sprintf("%s-funnel.png", dich_rma_prefix)
    dontpanic::msg(imgpath)
    png(imgpath)
    funnel(
      dich_pw_mod,
      main = this_title,
      atransf = exp,
      level = c(90, 95, 99),
      # refline = 0,
      shade = c("white", "gray55", "gray75")
    )

    dev.off()
  }),





  # dich nma ----------------------------------------------------------------

  tar_target(
    dich_nma_agd,
    dich_all_dat %>%
      mutate(intervention = str_to_sentence(intervention)) %>%
      viable_observations() %>%
      set_agd_arm(
        data = .,
        trt = intervention,
        r = r,
        n = n,
        sample_size = n,
        study = study,
        trt_class = class,
        trt_ref = "Placebo"
      )
  ),

  tar_target(
    dich_nma_mod,
    # nma(dich_nma_agd, trt_effects = "random")
    sprintf("exports/%s-nma.rds", dich_outcome) %>%
      read_rds()
  ),

  tar_target(dich_nma_write,
             if (FALSE) {
               write_rds(dich_nma_mod,
                         sprintf("exports/%s-nma.rds", dich_outcome))
             } else
               NULL),



  tar_target(
    dich_ints_output,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      group_by(.trt) %>%
      summarise(n = n_distinct(.study)) %>%
      arrange(desc(n)) %>%
      mutate(ints = glue("{.trt} ({n})")) %>%
      pull(ints) %>%
      paste(collapse = "; ") %>%
      str_to_sentence()
  ),

  tar_target(
    dich_conditions,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      group_by(condition) %>%
      summarise(n = n_distinct(.study)) %>%
      arrange(desc(n)) %>%
      mutate(con = glue("{condition} ({n})")) %>%
      pull(con) %>%
      paste(collapse = "; ")
  ),

  tar_target(
    dich_contrasts,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      group_by(.study) %>%
      arrange(.trt) %>%
      summarise(ints = unique(.trt) %>% paste(collapse = ";")) %>%
      filter(!str_detect(ints, "Placebo;")) %>%
      arrange(ints) %>%
      count(ints) %>%
      arrange(desc(n))
  ),




  # dich total --------------------------------------------------------------


  tar_target(dich_total, {
    totals <-
      dich_all_dat %>%
      mutate(is_placebo = if_else(intervention == "placebo",
                                  "placebo",
                                  "intervention")) %>%
      group_by(timepoint, is_placebo) %>%
      summarise(
        n = sum(n),
        studies = n_distinct(study),
        ints = n_distinct(intervention) - 1
      ) %>%
      ungroup() %>%
      pivot_wider(names_from = is_placebo,
                  values_from = c(n, studies, ints)) %>%
      mutate(
        n_total = n_intervention + n_placebo,
        studies_total = studies_intervention + studies_placebo,
        ints_total = ints_intervention + ints_placebo
      )

    totals %>%
      ungroup() %>%
      summarise(
        n_intervention = sum(n_intervention),
        n_placebo = sum(n_placebo),
        n_total = sum(n_total),
        studies_placebo = sum(studies_placebo),
        studies_intervention = sum(studies_intervention),
        studies_total = sum(studies_total),
        ints_placebo = sum(ints_placebo),
        ints_intervention = sum(ints_intervention),
        ints_total = sum(ints_total)
      ) %>%
      mutate(timepoint = "total") %>%
      bind_rows(totals, .) %>%
      filter(timepoint == "post_int")
  }),


  # gradepro ----------------------------------------------------------------


  tar_target(dich_gradepro,
             dich_all_int_dat),


  # dich sof ----------------------------------------------------------------


  tar_target(dich_sof_dat,
             {
               rel <-
                 dich_nma_mod %>%
                 relative_effects() %>%
                 as_tibble() %>%
                 clean_names() %>%
                 mutate(intervention = str_remove(parameter, "d\\[") %>%
                          str_remove("\\]")) %>%
                 select(
                   intervention,
                   rel_mean = mean,
                   rel_lb = x2_5_percent,
                   rel_ub = x97_5_percent
                 ) %>%
                 mutate(across(where(is.numeric), exp)) %>%
                 mutate(across(where(is.numeric), round, 2)) %>%
                 mutate(rel = glue("{rel_mean} ({rel_lb} to {rel_ub})"))



               rank <-
                 dich_nma_mod %>%
                 posterior_ranks(probs = c(0.025, 0.975),
                                 lower_better = outcome_dir(dich_outcome) == "lower") %>%
                 as_tibble() %>%
                 clean_names() %>%
                 mutate(intervention = str_remove(parameter, "rank\\[") %>%
                          str_remove("\\]")) %>%
                 select(
                   intervention,
                   rank_mean = mean,
                   rank_lb = x2_5_percent,
                   rank_ub = x97_5_percent
                 ) %>%
                 mutate(across(where(is.numeric), round, 0))  %>%
                 mutate(rank = glue("{rank_mean} ({rank_lb} to {rank_ub})"))



               full_join(
                 rank %>% select(intervention, rank)  %>%
                   filter(intervention != "Placebo"),
                 rel %>% select(intervention, rel)
               ) %>%
                 mutate(
                   rel = if_else(intervention == "Placebo",
                                 "", as.character(rel)),
                   studies = map_int(
                     intervention,
                     ~ dich_nma_mod$network$agd_arm %>% filter(.trt == .x) %>% pull(.study) %>% n_distinct()
                   ),
                   participants = map_chr(
                     intervention,
                     .f = function(int) {
                       int_studies <-
                         dich_nma_mod$network$agd_arm %>%
                         filter(.trt == int) %>%
                         pull(.study) %>% unique()

                       dat <-
                         dich_nma_mod$network$agd_arm %>%
                         filter(.study %in% int_studies,
                                .trt %in% c(int, "Placebo"))

                       placebo <-
                         dat %>% filter(.trt == "Placebo") %>% pull(.n) %>% sum()
                       int <-
                         dat %>% filter(.trt == int) %>% pull(.n) %>% sum()
                       totes <- placebo + int
                       glue("{totes} ({placebo}, {int})")
                     }
                   ),
                   rob = map_dbl(
                     intervention,
                     .f = function(int) {
                       this_rob <-
                         dich_nma_mod$network$agd_arm %>%
                         filter(.trt == int) %>%
                         group_by(rob) %>%
                         summarise(study = n_distinct(.study)) %>%
                         mutate(total = sum(study), p = study / total) %>%
                         filter(rob == "high") %>% pull(p) %>%
                         round(2)

                       ifelse(length(this_rob) != 1, 0, this_rob)
                     }
                   )
                 ) %>%
                 arrange(desc(studies)) %>%
                 bind_rows(rank %>% filter(intervention == "Placebo") %>%
                             select(intervention, rank)) %>%
                 select(intervention, studies, participants, rel, rank, rob) %>%
                 mutate(participants = if_else(intervention == "Placebo", NA_character_, participants)) %>%
                 rename_with(str_to_sentence, everything()) %>%
                 mutate(across(everything(), replace_na, ""))
             }),

  tar_target(dich_sof_output, {
    this_sof <-
      dich_sof_dat %>%
      gt() %>%
      cols_label(
        Rank = "Rank (95% CI)",
        Rel = "Relative effect (95% CI)",
        Participants = "Participants (placebo, intervention)",
        Rob = "ROB"
      )  %>%
      # tab_header(
      #   title = glue("{outcome_label(dich_outcome)}") %>%
      #     str_to_sentence(),
      #   subtitle = "Summary of findings"
      # ) %>%
      tab_footnote(
        glue("Direction of improvement: {outcome_dir(dich_outcome)}."),
        # locations = cells_title("title")
        locations = cells_column_labels(columns = c(Rel, Rank))
      ) %>%
      tab_footnote(footnote = "Proportion of studies with at least one with high risk of bias (ROB) criterion.",
                   locations = cells_column_labels(columns = Rob)) %>%
      tab_footnote(footnote = "Table is ordered by interventions with greatest number of studies.",
                   locations = cells_column_labels(columns = c(Studies, Participants))) %>%
      tab_source_note(
        "This table follows the recommendations of Yepes-Nuñez, J. J., Li, S.-A., Guyatt, G., Jack, S. M., Brozek, J. L., Beyene, J., Murad, M. H., Rochwerg, B., Mbuagbaw, L., Zhang, Y., Flórez, I. D., Siemieniuk, R. A., Sadeghirad, B., Mustafa, R., Santesso, N., & Schünemann, H. J. (2019). Development of the summary of findings table for network meta-analysis. Journal of Clinical Epidemiology, 115, 1–13. https://doi.org/10.1016/j.jclinepi.2019.04.018"
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "white",
          style = "solid",
          weight = px(1)
        ),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      opt_row_striping() %>%
      opt_table_lines("none") %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_labels()) %>%
      tab_options(table.width = px(800),
                  table.font.size = 12)



    gtsave(this_sof,
           sprintf("report/img/%s-sof.png", dich_outcome))

  }),

  # dich pico --------------------------------------------------------------------

  tar_target(
    dich_pico_design,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      group_by(design) %>%
      summarise(n = n_distinct(.study)) %>%
      mutate(txt = glue("{design} ({n})")) %>%
      pull(txt) %>%
      paste(collapse = "; ")
  ),

  tar_target(
    dich_pico_timerange,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      group_by(design) %>%
      pull(weeks) %>% {
        sprintf(
          "Studies ran from %d to %d weeks, with a median length of %d weeks",
          min(.),
          max(.),
          median(.)
        )
      }
  ),

  tar_target(
    dich_pico_dat,
    dich_nma_mod %>%
      pluck("network", "agd_arm") %>%
      arrange(.trt) %>%
      summarise(
        outcome = unique(outcome),
        n_studies = n_distinct(.study),
        participants = sum(n),
        n_con = n_distinct(condition),
        n_int = n_distinct(.trt) - 1,
      ) %>%
      mutate(
        dir = outcome_dir(dich_outcome),
        measure = timepoint_label(dich_timepoint),
        model = if_else(
          outcome_mod(dich_outcome) == "smd",
          "Standardised mean difference",
          "Odds ratio"
        ),
        conditions = dich_conditions,
        ints = dich_ints_output,
        outcome = outcome_label(dich_outcome),
        design = dich_pico_design,
        timerange = dich_pico_timerange
      ) %>%
      mutate(across(everything(), str_to_sentence)) %>%
      select(
        outcome,
        measure,
        model,
        n_studies,
        participants,
        contains("con"),
        contains("int"),
        everything()
      ) %>%
      # too big for pico put in text instead
      select(-ints) %>%
      rename(
        "Number of studies" = n_studies,
        "Number of conditions" = n_con,
        "Number of interventions" = n_int,
        "Conditions (n)" = conditions,
        # "Interventions (n)" = ints,
        "Design (n)" = design,
        "Time range" = timerange,
        "Direction of improvement" = dir,
        "Model estimate" = model
      )
  ),

  tar_target(dich_pico, {
    this_gt <-
      dich_pico_dat %>%
      t() %>%
      as_tibble(rownames = "cat") %>%
      mutate(across(everything(), str_to_sentence)) %>%
      rename(txt = V1) %>%
      mutate(across(everything(), str_replace, "Pgic", "PGIC:")) %>%
      gt() %>%
      cols_width(cat ~ px(300)) %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "white",
          style = "solid",
          weight = px(1)
        ),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_style(
        style = cell_text(v_align = "top"),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_style(style = cell_text(weight = "bold", align = "right"),
                cells_body(columns = cat)) %>%
      opt_table_lines("none") %>%
      tab_footnote("Number of studies (n).",
                   locations = cells_body(columns = cat,
                                          rows =  str_detect(cat, "\\("))) %>%
      tab_options(
        table.font.size = 20,
        table.width = px(700),
        column_labels.hidden = TRUE
      )

    sprintf("report/img/%s-pico.png", dich_outcome) %>%
      gtsave(this_gt, .)
  }),

  # postint and change score ------------------------------------------------
  # set cspi ----------------------------------------------------------------

  tar_target(cspi_outcome,
             "sleep"),

  tar_target(cspi_timepoint,
             "post_int"),

  tar_target(cspi_subgroup,
             " "),

  tar_target(cspi_intervention,
             "amitriptyline"),

  tar_target(cspi_comparator,
             "placebo"),


  # cspi nma ---------------------------------------------------------------




  tar_target(
    cspi_dat,
    mod_dat %>%
      filter(
        outcome == cspi_outcome,
        timepoint %in% c("change_score", "post_int")
      ) %>%
      mutate(
        intervention = fct_relevel(intervention, "placebo",
                                   "cbt",
                                   "cbt and milnacipran"),
        class = toupper(class)
      ) %>%
      arrange(study, intervention) %>%
      group_by(timepoint, study) %>%
      mutate(
        arm = 1:n(),
        mi = sum(n) - n(),
        cmi = exp(lgamma(mi / 2) - log(sqrt(mi / 2)) - lgamma((mi - 1) /
                                                                2)),
        sdpool = sqrt(weighted.mean(sd ^ 2, n - 1)),
        smd = if_else(arm == 1, NA_real_, (mean - first(mean)) / sdpool * cmi),
        se_smd = if_else(arm == 1,
                         se / sdpool * cmi,
                         sqrt((n + first(
                           n
                         )) / (n * first(
                           n
                         )) + smd ^ 2 / (2 * (
                           n + first(n)
                         ))))
      ) %>%
      select(study, intervention, smd, se_smd, n, everything()) %>%
      mutate(intervention = str_to_sentence(intervention))
  ),


  tar_target(
    cspi_net_cs,
    set_agd_contrast(
      data = cspi_dat %>%
        filter(timepoint == "change_score") %>%
        viable_observations(),
      study = study,
      trt = intervention,
      y = smd,
      se = se_smd,
      sample_size = n,
      trt_class = class,
      trt_ref = "Placebo"
    )
  ),

  tar_target(
    cspi_net_pi,
    set_agd_contrast(
      data = cspi_dat %>%
        filter(timepoint == "post_int")  %>%
        viable_observations(),
      study = study,
      trt = intervention,
      y = smd,
      se = se_smd,
      sample_size = n,
      trt_class = class,
      trt_ref = "Placebo"
    )
  ),

  # read nma ---------------------------------------------------------------

  tar_target(
    cspi_nma_cs,
    # NULL
    # nma(cspi_net_cs, trt_effects = "random")
    sprintf("exports/%s-change_score-nma.rds", cspi_outcome) %>% read_rds()
  ),

  tar_target(
    cspi_nma_pi,
    # nma(cspi_net_pi, trt_effects = "random")
    sprintf("exports/%s-post_int-nma.rds", cspi_outcome) %>% read_rds()
  ),

  tar_target(cspi_update_mod,
             FALSE),

  tar_target(cspi_nma_write,
             if (cspi_update_mod) {
               write_rds(cspi_nma_pi,
                         sprintf("exports/%s-post_int-nma.rds",
                                 cspi_outcome))
               write_rds(cspi_nma_cs,
                         sprintf("exports/%s-change_score-nma.rds",
                                 cspi_outcome))
             } else {
               NULL
             }),

  # get totals --------------------------------------------------------------



  tar_target(cspi_total, {
    totals <-
      cspi_dat %>%
      mutate(is_placebo = if_else(intervention == "Placebo",
                                  "placebo",
                                  "intervention")) %>%
      group_by(timepoint, is_placebo) %>%
      summarise(
        n = sum(n),
        studies = n_distinct(study),
        ints = n_distinct(intervention) - 1
      ) %>%
      ungroup() %>%
      pivot_wider(names_from = is_placebo,
                  values_from = c(n, studies, ints)) %>%
      mutate(
        n_total = n_intervention + n_placebo,
        studies_total = studies_intervention + studies_placebo,
        ints_total = ints_intervention + ints_placebo
      )

    totals %>%
      ungroup() %>%
      summarise(
        n_intervention = sum(n_intervention),
        n_placebo = sum(n_placebo),
        n_total = sum(n_total),
        studies_placebo = sum(studies_placebo),
        studies_intervention = sum(studies_intervention),
        studies_total = sum(studies_total),
        ints_placebo = sum(ints_placebo),
        ints_intervention = sum(ints_intervention),
        ints_total = sum(ints_total)
      ) %>%
      mutate(timepoint = "total") %>%
      bind_rows(totals, .)
  }),



  tar_target(cspi_contrasts, {
    these_studies <-
      cspi_dat %>%
      filter(intervention != "placebo") %>%
      pull(study) %>% unique()

    cspi_dat %>%
      filter(study %in% these_studies) %>%
      group_by(study, timepoint) %>%
      arrange(intervention) %>%
      summarise(ints = intervention %>% unique() %>% tolower() %>%  paste(collapse = '; ')) %>%
      filter(!str_detect(ints, "placebo")) %>%
      arrange(timepoint) %>%
      group_by(timepoint) %>%
      count(ints) %>%
      arrange(timepoint, desc(n))

  }),

  tar_target(cspi_combns, {
    these_studies <-
      cspi_dat %>%
      filter(intervention != "placebo") %>%
      pull(study) %>% unique()

    this_int <- cspi_intervention

    cspi_dat %>%
      filter(study %in% these_studies) %>%
      group_by(study, timepoint) %>%
      arrange(intervention) %>%
      summarise(ints = intervention %>% unique() %>% tolower() %>%  paste(collapse = '; ')) %>%
      filter(ints != glue("{this_int}; placebo"), ints != "placebo")

  }),

  tar_target(
    cspi_lists,
    cspi_dat %>%
      filter(intervention != "Placebo") %>%
      group_by(timepoint) %>%
      summarise(
        int_n = n_distinct(intervention),
        ints = intervention %>% unique() %>% paste(collapse = "; "),
        con_n = n_distinct(condition),
        cons = condition %>% unique() %>% paste(collapse = "; ")
      )
  ),


  # set cspi nma ------------------------------------------------------------


  tar_target(cspi_nma_mod,
             if (cspi_timepoint == "change_score")
               cspi_nma_cs
             else
               cspi_nma_pi),



  # cspi sof ----------------------------------------------------------------



  tar_target(cspi_sof_dat,
             {
               rel <-
                 cspi_nma_mod %>%
                 relative_effects() %>%
                 as_tibble() %>%
                 clean_names() %>%
                 mutate(intervention = str_remove(parameter, "d\\[") %>%
                          str_remove("\\]")) %>%
                 select(
                   intervention,
                   rel_mean = mean,
                   rel_lb = x2_5_percent,
                   rel_ub = x97_5_percent
                 ) %>%
                 mutate(across(where(is.numeric), round, 2)) %>%
                 mutate(rel = glue("{rel_mean} ({rel_lb} to {rel_ub})"))



               rank <-
                 cspi_nma_mod %>%
                 posterior_ranks(probs = c(0.025, 0.975),
                                 lower_better = outcome_dir(cspi_outcome) == "lower") %>%
                 as_tibble() %>%
                 clean_names() %>%
                 mutate(intervention = str_remove(parameter, "rank\\[") %>%
                          str_remove("\\]")) %>%
                 select(
                   intervention,
                   rank_mean = mean,
                   rank_lb = x2_5_percent,
                   rank_ub = x97_5_percent
                 ) %>%
                 mutate(across(where(is.numeric), round, 0))  %>%
                 mutate(rank = glue("{rank_mean} ({rank_lb} to {rank_ub})"))



               full_join(
                 rank %>%
                   filter(intervention != "Placebo") %>%
                   select(intervention, rank),
                 rel %>% select(intervention, rel)
               ) %>%
                 mutate(
                   rel = if_else(intervention == "Placebo",
                                 "", as.character(rel)),
                   studies = map_int(
                     intervention,
                     ~ cspi_nma_mod$network$agd_contrast %>% filter(.trt == .x) %>% pull(.study) %>% n_distinct()
                   ),
                   participants = map_chr(
                     intervention,
                     .f = function(int) {
                       int_studies <-
                         cspi_nma_mod$network$agd_contrast %>%
                         filter(.trt == int) %>%
                         pull(.study) %>% unique()

                       dat <-
                         cspi_nma_mod$network$agd_contrast %>%
                         filter(.study %in% int_studies,
                                .trt %in% c(int, "Placebo"))

                       placebo <-
                         dat %>% filter(.trt == "Placebo") %>% pull(n) %>% sum()
                       int <-
                         dat %>% filter(.trt == int) %>% pull(n) %>% sum()
                       totes <- placebo + int
                       glue("{totes} ({placebo}, {int})")
                     }
                   ),
                   rob = map_dbl(
                     intervention,
                     .f = function(int) {
                       this_rob <-
                         cspi_nma_mod$network$agd_contrast %>%
                         filter(.trt == int) %>%
                         group_by(rob) %>%
                         summarise(study = n_distinct(.study)) %>%
                         mutate(total = sum(study), p = study / total) %>%
                         filter(rob == "high") %>% pull(p) %>%
                         round(2)

                       ifelse(length(this_rob) != 1, 0, this_rob)
                     }
                   )
                 ) %>%
                 arrange(desc(studies)) %>%
                 bind_rows(rank %>% filter(intervention == "Placebo") %>%
                             select(intervention, rank)) %>%
                 mutate(participants = if_else(intervention == "Placebo", NA_character_, participants)) %>%
                 select(intervention, studies, participants, rel, rank, rob) %>%
                 rename_with(str_to_sentence, everything()) %>%
                 mutate(across(everything(), replace_na, ""))
             }),

  tar_target(cspi_sof_output, {
    this_sof <-
      cspi_sof_dat %>%
      gt() %>%
      cols_label(
        Rank = "Rank (95% CI)",
        Rel = "Relative effect (95% CI)",
        Participants = "Participants (placebo, intervention)",
        Rob = "ROB"
      )  %>%
      # tab_header(
      #   title = glue("{outcome_label(cspi_outcome)}") %>%
      #     str_to_sentence(),
      #   subtitle = "Summary of findings"
      # ) %>%
      tab_footnote(
        glue("Direction of improvement: {outcome_dir(cspi_outcome)}."),
        # locations = cells_title("title")
        locations = cells_column_labels(columns = c(Rel, Rank))
      ) %>%
      tab_footnote(footnote = "Proportion of studies with at least one with high risk of bias (ROB) criterion.",
                   locations = cells_column_labels(columns = Rob)) %>%
      tab_footnote(footnote = "Table is ordered by interventions with greatest number of studies.",
                   locations = cells_column_labels(columns = c(Studies, Participants))) %>%
      tab_source_note(
        "This table follows the recommendations of Yepes-Nuñez, J. J., Li, S.-A., Guyatt, G., Jack, S. M., Brozek, J. L., Beyene, J., Murad, M. H., Rochwerg, B., Mbuagbaw, L., Zhang, Y., Flórez, I. D., Siemieniuk, R. A., Sadeghirad, B., Mustafa, R., Santesso, N., & Schünemann, H. J. (2019). Development of the summary of findings table for network meta-analysis. Journal of Clinical Epidemiology, 115, 1–13. https://doi.org/10.1016/j.jclinepi.2019.04.018"
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "white",
          style = "solid",
          weight = px(1)
        ),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      opt_row_striping() %>%
      opt_table_lines("none") %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_labels()) %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "white",
          style = "solid",
          weight = px(1)
        ),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_options(table.width = px(800),
                  table.font.size = 12)



    gtsave(this_sof,
           sprintf("report/img/%s-%s-sof.png", cspi_outcome, cspi_timepoint))

  }),


  # cspi meta-analysis ------------------------------------------------------

  tar_target(
    cspi_pw_escalc,
    cspi_dat %>%
      filter(
        timepoint == cspi_timepoint,!is.na(smd),
        intervention %in% str_to_sentence(c(cspi_intervention, cspi_comparator))
      ) %>%
      ungroup()
  ),

  tar_target(
    cspi_pw_mod,
    cspi_pw_escalc %>%
      rma(
        yi = smd,
        vi = se_smd,
        slab = study,
        data = .,
        measure = "SMD"
      )
  ),

  tar_target(
    cspi_rma_prefix,
    sprintf(
      "report/img/%s-%s-%s-%s-%s",
      cspi_outcome,
      cspi_timepoint,
      cspi_subgroup,
      cspi_intervention,
      cspi_comparator
    ) %>%
      str_remove("\\s-")
  ),



  # cspi pico --------------------------------------------------------------------
  tar_target(
    cspi_ints_output,
    cspi_nma_mod %>%
      pluck("network", "agd_contrast") %>%
      group_by(.trt) %>%
      summarise(n = n_distinct(.study)) %>%
      arrange(desc(n)) %>%
      mutate(ints = glue("{.trt} ({n})")) %>%
      pull(ints) %>%
      paste(collapse = "; ") %>%
      str_to_sentence()
  ),


  tar_target(
    cspi_conditions,
    cspi_nma_mod %>%
      pluck("network", "agd_contrast") %>%
      group_by(condition) %>%
      summarise(n = n_distinct(.study)) %>%
      arrange(desc(n)) %>%
      mutate(con = glue("{condition} ({n})")) %>%
      pull(con) %>%
      paste(collapse = "; ")
  ),


  tar_target(
    cspi_pico_design,
    cspi_nma_mod %>%
      pluck("network", "agd_contrast") %>%
      group_by(design) %>%
      summarise(n = n_distinct(.study)) %>%
      mutate(txt = glue("{design} ({n})")) %>%
      pull(txt) %>%
      paste(collapse = "; ")
  ),

  tar_target(
    cspi_pico_timerange,
    cspi_nma_mod %>%
      pluck("network", "agd_contrast") %>%
      group_by(design) %>%
      pull(weeks) %>% {
        sprintf("Studies ran from %d to %d weeks, with median length %d weeks",
                min(.),
                max(.),
                median(.))
      }
  ),

  tar_target(
    cspi_pico_dat,
    cspi_nma_mod %>%
      pluck("network", "agd_contrast") %>%
      arrange(.trt) %>%
      summarise(
        outcome = unique(outcome),
        n_studies = n_distinct(.study),
        participants = sum(n),
        n_con = n_distinct(condition),
        n_int = n_distinct(.trt) - 1,
      ) %>%
      mutate(
        measure = timepoint_label(cspi_timepoint),
        estimate = "Standardised mean difference",
        dir = outcome_dir(cspi_outcome),
        conditions = cspi_conditions,
        ints = cspi_ints_output,
        outcome = outcome_label(cspi_outcome),
        design = cspi_pico_design,
        timerange = cspi_pico_timerange
      ) %>%
      mutate(across(everything(), str_to_sentence)) %>%
      select(
        outcome,
        measure,
        estimate,
        n_studies,
        participants,
        contains("con"),
        # contains("int"),
        everything()
      ) %>%
      rename(
        "Model estimate" = estimate,
        "Number of studies" = n_studies,
        "Number of conditions" = n_con,
        "Number of interventions" = n_int,
        "Conditions (n)" = conditions,
        # "Interventions (n)" = ints,
        "Design (n)" = design,
        "Time range" = timerange,
        "Direction of improvement" = dir
      )
  ),

  tar_target(cspi_pico, {
    this_gt <-
      cspi_pico_dat %>%
      select(-ints) %>%
      t() %>%
      as_tibble(rownames = "cat") %>%
      mutate(across(everything(), str_to_sentence)) %>%
      rename(txt = V1) %>%
      mutate(across(everything(), str_replace, "Pgic", "PGIC:")) %>%
      gt() %>%
      cols_width(cat ~ px(300)) %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "white",
          style = "solid",
          weight = px(1)
        ),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_style(
        style = cell_text(v_align = "top"),
        locations = cells_body(columns = everything(),
                               rows = everything())
      ) %>%
      tab_style(style = cell_text(weight = "bold", align = "right"),
                cells_body(columns = cat)) %>%
      opt_table_lines("none") %>%
      tab_footnote("Number of studies (n).",
                   locations = cells_body(columns = cat,
                                          rows =  str_detect(cat, "\\("))) %>%
      tab_options(
        table.font.size = 20,
        table.width = px(700),
        column_labels.hidden = TRUE,
        column_labels.border.bottom.width = 0,
        column_labels.border.lr.width = 0
      )

    sprintf("report/img/%s-%s-pico.png", cspi_outcome, cspi_timepoint) %>%
      gtsave(this_gt, .)
  }),



  # cspi combine ------------------------------------------------------------

  tar_target(cspi_cs_dat, {
    relative_effects(cspi_nma_cs, probs = c(0.025, 0.975)) %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(intervention = str_remove(parameter, "d\\[") %>%
               str_remove("\\]"),
             tp = "cs") %>%
      select(intervention,
             mean,
             sd,
             tp,
             ci_lb = x2_5_percent,
             ci_ub = x97_5_percent)
  }),

  tar_target(cspi_pi_dat, {
    relative_effects(cspi_nma_pi, probs = c(0.025, 0.975)) %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(intervention = str_remove(parameter, "d\\[") %>%
               str_remove("\\]"),
             tp = "pi") %>%
      select(intervention,
             mean,
             sd,
             tp,
             ci_lb = x2_5_percent,
             ci_ub = x97_5_percent)
  }),

  tar_target(
    cspi_cspi_dat,
    bind_rows(cspi_cs_dat, cspi_pi_dat) %>%
      group_by(intervention) %>%
      group_split() %>%
      purrr::discard(~ nrow(.x) < 2)
  ),

  tar_target(cspi_comb_rma,
             cspi_cspi_dat %>%
               map(mutate, var = sd^2) %>%
               map(~rma(
                 yi = mean,
                 vi = var,
                 data = .x
               ))
               ,
             ),

  tar_target(
    cspi_comb_rels,
    cspi_cspi_dat %>%
      bind_rows() %>%
      pivot_wider(
        id_cols = intervention,
        names_from = tp,
        names_glue = "{.value}_{tp}",
        values_from = -c(intervention, tp)
      )
  ),

  tar_target(cspi_comb_output_dat,
             tibble(
               intervention = cspi_cspi_dat %>%
                 map("intervention") %>%
                 map_chr(1)
             ) %>%
               mutate(
                 mod = cspi_comb_rma,
                 mean_cspi = map_dbl(mod, "beta"),
                 ci_lb_cspi = map_dbl(mod, "ci.lb"),
                 ci_ub_cspi = map_dbl(mod, "ci.ub")
               ) %>%
               full_join(
                 cspi_comb_rels
               )
             ),

  tar_target(cspi_comb_gt,
             cspi_comb_output_dat %>%
               select(-contains('mod')) %>%
               mutate(across(where(is.double), round, 2)) %>%
               mutate(
                 cs = str_c(mean_cs, " (",
                            ci_lb_cs,
                            " to ",
                            ci_ub_cs,
                            ")"),
                 pi = str_c(mean_pi, " (",
                            ci_lb_pi,
                            " to ",
                            ci_ub_pi,
                            ")"),
                 cspi = str_c(mean_cspi, " (",
                            ci_lb_cspi,
                            " to ",
                            ci_ub_cspi,
                            ")")
               ) %>%
               select(
                 intervention, cs,
                 pi, cspi
               ) %>%
               gt() %>%
               cols_label(
                 intervention = "Intervention",
                 cs = "Change score",
                 pi = "Post intervention",
                 cspi = "Weighted average"
               ) %>%
               tab_footnote(
                 "Estimate (95% Credible interval)",
                 locations = cells_column_labels(
                   columns = -intervention
                 )
               ) %>%
               tab_header(
                 subtitle = "Weighted average of change score and post-intervention relative effects compared with placebo",
                 title = outcome_label(cspi_outcome) %>% str_to_sentence())
             ),

  tar_target(
    cspi_comb_write, {
      img_path <- sprintf(
        "report/img/%s-combined.png",
        cspi_outcome
      )

      dontpanic::msg(img_path)

      gtsave(cspi_comb_gt, img_path)
    }
  ),

  # set reporting things ----------------------------------------------------

  tar_target(rep_mod,
             "cspi"),

  # dependencies ------------------------------------------------------------

  tar_target(rep_outcome,
             if (rep_mod == "dich")
               dich_outcome
             else
               cspi_outcome),

  tar_target(rep_intervention,
             if (rep_mod == "dich")
               dich_intervention
             else
               cspi_intervention),

  tar_target(rep_comparator,
             if (rep_mod == "dich")
               dich_comparator
             else
               cspi_comparator),

  tar_target(rep_timepoint,
             if (rep_mod == "dich")
               dich_timepoint
             else
               cspi_timepoint),

  tar_target(rep_subgroup,
             if (rep_mod == "dich")
               dich_subgroup
             else
               cspi_subgroup),

  tar_target(rep_nma,
             if (rep_mod == "dich")
               dich_nma_mod
             else
               cspi_nma_mod),

  tar_target(rep_regtest,
             if (rep_mod == "dich")
               dich_regtest
             else
               cspi_regtest),

  tar_target(rep_pw_mod,
             if (rep_mod == "dich")
               dich_pw_mod
             else
               cspi_pw_mod),



  tar_target(
    rep_main_aim,
    mod_dat %>%
      select(outcome, study, main_aim = main_aim_of_study) %>%
      distinct()
  ),


  tar_target(rep_pw_escalc, {
    this_dat <-
      if (outcome_mod(rep_outcome) == "lor")
        dich_pw_escalc
    else
      cspi_pw_escalc

    this_dat %>%
      left_join(rep_main_aim)
  }),


  tar_target(rep_rma_prefix,
             if (outcome_mod(rep_outcome) == "smd")
               cspi_rma_prefix
             else
               dich_rma_prefix),

  tar_target(rep_nma_dat, {
    if (outcome_mod(rep_outcome) == "smd") {
      rep_nma$network$agd_contrast
    } else
      rep_nma$network$agd_arm
  }),


  # create model tag --------------------------------------------------------

  tar_target(
    rep_label,
    sprintf("report/img/%s-%s",
            rep_outcome,
            rep_timepoint)
  ),


  # nma reporting -----------------------------------------------------------

  tar_target(rep_net, {
    plot <-
      plot(
        rep_nma$network,
        weight_nodes = TRUE,
        weight_edges = TRUE,
        show_trt_class = TRUE
      ) +
      ggplot2::theme(
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.box = "vertical"
      )

    sprintf("%s-net.png", rep_label) %>%
      ggsave(dpi = "retina")
  }),

  tar_target(
    rep_participants,
    rep_nma_dat %>%
      mutate(is_placebo = if_else(
        .trt == "Placebo",
        "placebo",
        "intervention"
      )) %>%
      group_by(is_placebo) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      pivot_wider(names_from = is_placebo, values_from = n) %>%
      mutate(total = intervention + placebo) %>%
      gt() %>%
      summary_rows(
        groups = FALSE,
        columns = n,
        fns = list(total = ~ sum(.x))
      )
  ),


  # rep pico ----------------------------------------------------------------




  # pw reporting ------------------------------------------------------------

  tar_target(pw_participants,
             # tar_read(cspi_dat) %>%
             #   pull(n) %>% sum()
             rep_pw_mod %>% pluck("ni") %>% sum()),


  tar_target(rep_pw_caption, {
    modeltext <-
      sprintf(
        "Outcome: %s. Measure: %s. Intervention (%s) compared with (%s)",
        outcome_label(rep_outcome) %>% str_replace("%", " per cent"),
        timepoint_label(rep_timepoint),
        rep_intervention,
        rep_comparator
      )

    # include all regtest results
    regtext <-
      tibble(
        est = rep_regtest$est,
        ci_lb = rep_regtest$ci.lb,
        ci_ub = rep_regtest$ci.ub,
        p = rep_regtest$pval,
        z = rep_regtest$zval,
        sig = ci_ub < 0 | ci_lb > 0,
        sig_text = if_else(sig,
                           "significant",
                           "not significant")
      ) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      mutate(
        regtest =
          glue(
            "Regression test for funnel plot asymmetry was {sig_text}: {est} (CI {ci_lb} to {ci_ub}) with p = {p} and z = {z}."
          )
      ) %>% pull(regtest) %>% unique()

    # get foresttext
    forest_text <-
      tibble(
        q = rep_pw_mod$QE,
        df = rep_pw_mod$k - rep_pw_mod$p,
        p = metafor:::.pval(
          rep_pw_mod$QEp,
          digits = 2,
          showeq = TRUE,
          sep = " "
        ),
        i_sq = rep_pw_mod$I2,
        tau_sq = rep_pw_mod$tau2
      ) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      mutate(
        text =
          glue(
            "Meta-analysis results: Q = {q}; df = {df}; p {p}; $I^2$ = {i_sq} per cent; $\\tau^2$ = {tau_sq}."
          )
      ) %>% pull(text) %>%
      str_replace("<", "$<$")

    participants <-
      pw_participants

    dir <-
      glue("Direction of improvement: {outcome_dir(rep_outcome)}.")



    txt <-
      # glue("{modeltext} Participants: {participants}. {dir} {forest_text} {regtext}")
      sprintf("%s Participants: %s. %s %s %s",
              modeltext,
              participants,
              dir,
              forest_text,
              regtext)

    pth <-
      sprintf("%s-rma-caption.txt", rep_rma_prefix)

    dontpanic::msg(pth)

    write_file(txt, pth)
  }),

  # pw forests ------------------------------------------------------------


  # funnel ------------------------------------------------------------------

  tar_target(rep_funnel, {
    if (str_length(rep_subgroup) < 2) {
      this_title <- sprintf("%s: %s vs %s",
                            outcome_label(rep_outcome),
                            rep_intervention,
                            rep_comparator)

      dontpanic::msg(this_title)

      img_path <-
        sprintf("%s-funnel.png", rep_rma_prefix)

      dontpanic::msg(img_path)

      png(img_path)


      # don't forget totransform for exp
      funnel(
        rep_pw_mod,
        main = this_title,
        level = c(90, 95, 99),
        # transf = exp,
        # refline = 0,
        shade = c("white", "gray55", "gray75")
      )

      dev.off()
    }
  }),

  tar_target(rep_pw_forest, {
    if (str_length(rep_subgroup) < 2) {
      this_title <- sprintf("%s: %s vs %s",
                            outcome_label(rep_outcome),
                            rep_intervention,
                            rep_comparator)

      dontpanic::msg(this_title)

      img_path <-
        sprintf("%s-forest.png", rep_rma_prefix)

      dontpanic::msg(img_path)

      png(img_path)


      # don't forget totransform for exp
      forest(rep_pw_mod,
             # transf = exp,
             # refline = 02
             main = this_title)

      dev.off()
    }
  }),


  # pw subgroups ------------------------------------------------------------



  tar_target(rep_pw_cspi, {
    # need to make a dataset for labels

    # get bits for forest

    # get intervention as went into model
    all_dat <-
      cspi_dat %>%
      filter(timepoint == cspi_timepoint) %>%
      select(
        timepoint,
        study,
        intervention,
        mean,
        sd,
        n,
        contains('smd'),
        dose,
        duration,
        rob,
        condition,
        design
      ) %>%
      mutate(intervention = tolower(intervention))


    all_dat %>%
      filter(intervention == cspi_comparator) %>%
      rename(
        control = intervention,
        mean_control = mean,
        sd_control = sd,
        n_control = n
      ) %>%
      select(study, contains("control")) %>%
      left_join(
        all_dat %>%
          filter(
            intervention != cspi_comparator,
            intervention == cspi_intervention
          ),
        by = c("study", "timepoint")
      ) %>%
      filter(!is.na(mean)) %>%
      ungroup()
  }),

  tar_target(rep_pw_input_dat, {
    if (outcome_mod(rep_outcome) == "smd") {
      rep_pw_cspi %>%
        left_join(rep_main_aim %>% rename(main_aim_of_study = main_aim))

    } else {
      rep_pw_escalc
    }
  }),



  # levels and subgroups ----------------------------------------------------


  tar_target(rep_pw_levels,
             rep_pw_input_dat %>%
               count(main_aim)),


  tar_target(rep_pw_subgroups, {
    if (outcome_mod(rep_outcome) == "lor") {
      # get subgroups for all
      rep_nma %>%
        pluck("network", "agd_arm") %>%
        group_by(.trt) %>%
        count(main_aim_of_study) %>%
        filter(n > 1, .trt != "Placebo") %>%
        group_split(.trt) %>%
        purrr::discard( ~ nrow(.x) < 2) %>%
        purrr::discard( ~ sum(.x$n) < 4)
    } else {
      cspi_dat %>%
        group_by(timepoint, intervention) %>%
        count(main_aim_of_study) %>%
        filter(n > 1,
               intervention != "Placebo") %>%
        group_split() %>%
        purrr::discard( ~ nrow(.x) < 2) %>%
        purrr::discard( ~ sum(.x$n) < 4)

    }
  }),

  # make write forest plot --------------------------------------------------


  tar_target(rep_pw_forgroup_write, {
    if (str_length(rep_subgroup) > 2) {
      ### a little helper function to add Q-test, I^2, and tau^2 estimate info
      imgpath <-
        sprintf("%s-forest.png",
                rep_rma_prefix)

      dontpanic::msg("Make subgroup plot go!")


      # need to adjust this width
      png(imgpath,
          width = 800,
          height = 1000)



      rep_forgroup(
        rep_outcome,
        rep_timepoint,
        rep_intervention,
        rep_comparator,
        rep_subgroup,
        rep_rma_prefix,
        rep_pw_input_dat %>% mutate(main_aim = if_else(main_aim == "pain", main_aim, "other")),
        a_levels = 11,
        b_levels = 4,
        c_levels = NA,
        a_label =
          # "fibromyalgia",
          # "PI < 12",
          # "high",
          "pain",
        b_label =
          # "PI > 12",
          # "musculoskeletal",
          # "standard",
          "other",
        c_label =
          # "other"
          NA
        # "neuropathic",
        # "low"
        # "unable to be categorised"
      )


      dev.off()

      dontpanic::msg(imgpath)
    } else {
      NULL
    }
  }),


  # baseline densities ------------------------------------------------------

  tar_target(
    cspi_baseline_dat,
    mod_dat %>%
      filter(model_type == "smd") %>%
      mutate(
        intervention = fct_relevel(intervention, "placebo",
                                   "cbt",
                                   "cbt and milnacipran"),
        class = toupper(class)
      ) %>%
      arrange(outcome, timepoint, study, intervention) %>%
      group_by(outcome, timepoint, study) %>%
      mutate(
        arm = 1:n(),
        mi = sum(n) - n(),
        cmi = exp(lgamma(mi / 2) - log(sqrt(mi / 2)) - lgamma((mi - 1) /
                                                                2)),
        sdpool = sqrt(weighted.mean(sd ^ 2, n - 1)),
        smd = if_else(arm == 1, NA_real_, (mean - first(mean)) / sdpool * cmi),
        se_smd = if_else(arm == 1,
                         se / sdpool * cmi,
                         sqrt((n + first(
                           n
                         )) / (n * first(
                           n
                         )) + smd ^ 2 / (2 * (
                           n + first(n)
                         ))))
      ) %>%
      select(study, intervention, smd, se_smd, n, everything()) %>%
      mutate(intervention = str_to_sentence(intervention)) %>%
      filter(
        intervention %in% c("Amitriptyline", "Milnacipran", "Duloxetine")
      )
    # %>%
    #   select(outcome, timepoint, study, smd, se_smd, n)
  ),


  tar_target(
    dich_baseline_groups,
    mod_dat %>%
      filter(model_type == "lor") %>%
      group_by(outcome, timepoint) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    dich_baseline_input,
    dich_baseline_groups %>%
      filter(intervention == dich_comparator) %>%
      select(study, r_control = r, n_control = n) %>%
      inner_join(dich_baseline_groups) %>%
      distinct(),
    pattern = map(dich_baseline_groups)
  ),


  tar_target(
    dich_baseline_escalc,
    escalc(
      ai = r,
      ci = r_control,
      n1i = n,
      n2i = n_control,
      data = dich_baseline_input,
      measure = "OR",
      slab = study
    ) %>%
      mutate(intervention = str_to_sentence(intervention)) %>%
      filter(
        intervention %in% c("Amitriptyline", "Milnacipran", "Duloxetine")
      )
  ),

  tar_target(
    dich_baseline_plot,
    dich_baseline_escalc %>%
      mutate(x = exp(yi)) %>%
      ggplot(aes(
        x = x,
        colour = intervention,
        fill = intervention
      )) +
      geom_density(alpha = 0.3) +
      facet_wrap(
        ~ str_to_sentence(outcome_label),
        scales = "free",
        labeller = label_wrap_gen(width = 15)
      ) +
      labs(
        title = "Density of OR measures",
        subtitle = "Count outcomes",
        x = "Odds ratio",
        y = "Density",
        caption = "There were only post-intervention measures for these outcomes."
      ) +
      scale_color_discrete("Intervention") +
      scale_fill_discrete("Intervention") +
      ggthemes::theme_tufte(base_size = 8) +
      theme(
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  ),

  tar_target(
    dich_baseline_write,
    ggsave("report/img/dich-baseline.png",
           dich_baseline_plot)
  ),

  tar_target(cspi_baseline_plot, {
    cspi_baseline_dat %>%
      ungroup() %>%
      ggplot(aes(x = smd,
                 colour = intervention,
                 fill = intervention)) +
      geom_density(alpha = 0.3) +
      # theme_minimal() +
      facet_grid(
        str_to_sentence(outcome_label) ~ str_to_sentence(timepoint_label),
        scales = "free",
        labeller = label_wrap_gen(width = 15)
      ) +
      labs(
        title = "Density of SMD measures",
        subtitle = "Continuous outcomes",
        x = "Standardised mean difference",
        y = "Density"
      ) +
      scale_color_discrete("Intervention") +
      scale_fill_discrete("Intervention") +
      ggthemes::theme_tufte(base_size = 10) +
      theme(
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  }),

  tar_target(
    cspi_baseline_write,
    ggsave("report/img/cspi-baseline.png",
           cspi_baseline_plot)
  ),


  # check nma forest ---------------------------------------------------------------


  # we will always want to have all-in nma

  tar_target(
    rep_nma_for_base,
    forest_multinma(rep_nma,
                    outcome_mod(rep_outcome),
                    outcome_dir(rep_outcome))
  ),

  tar_target(rep_nma_for,
             hpp_forest(
               rep_nma, mod_type = "lor", dir = "lower"
             )),

  tar_target(
    rep_nma_for_class,
    hpp_forest(
      rep_nma,
      mod_type = "lor",
      dir = "lower",
      this_class = "snri"
    )
  ),

  # head to head comparisons ------------------------------------------------
  tar_target(
    hth_groups,
    mod_dat %>%
      group_by(outcome, timepoint) %>%
      filter(timepoint %in% c("post_int", "change_score")) %>%
      select(outcome, timepoint, intervention) %>%
      distinct() %>%
      nest() %>%
      # head(3) %>% # for testing
      tar_group(),
    iteration = "group"
  ),


  tar_target(
    hth_ints,
    {
      hth_groups %>%
        mutate(comps = map(data,
                           ~ combn(.x$intervention,
                                   2, simplify = FALSE))) %>%
        select(-data) %>%
        unnest(comps) %>%
        mutate(
          comps = map(comps, as.character),
          comp = map_chr(comps, ~ .x[1]),
          int = map_chr(comps, ~ .x[2])
        ) %>%
        select(-comps) %>%
        filter(comp != int) %>%
        mutate(across(everything(), as.character))
    },
    pattern = map(hth_groups)
  ),

  tar_target(
    hth_dat,
    {
      this_dat <-
        mod_dat %>%
        filter(outcome == unique(hth_ints$outcome),
               timepoint == unique(hth_ints$timepoint))
      hth_ints %>%
        mutate(dat = map2(
          comp,
          int,
          ~ this_dat %>%
            filter(intervention %in% c(.x, .y)) %>%
            viable_observations()
        ),
        n_obs = map_int(dat, nrow)
        )

    },
    pattern = map(hth_ints)
  ),

  tar_target(
    hth_missing,
    hth_dat %>%
      filter(n_obs == 0)
  ),

  tar_target(
    hth_viable,
    hth_dat %>%
      filter(n_obs != 0) %>%
      mutate(
        mod = map_chr(outcome, outcome_mod)
      )
  ),

  tar_target(
    hth_smd,
    hth_viable %>%
      filter(
        mod == "smd"
      ) %>%
      mutate(
        mod_input = map(dat, smd_calc),
        mod = map(mod_input, ~rma(
          yi = smd,
          sei = se_smd,
          slab = study,
          data = .x,
          measure = "SMD"
        )),
        reg = map(mod, safe_regtest)
       ) %>%
      select(-tar_group) %>%
      ungroup()
  ),

  tar_target(
    hth_lor,
    hth_viable %>%
      filter(
        mod == "lor"
      )
  ),


  # null --------------------------------------------------------------------

  NULL
)
