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
  # asserts that are currently failing
  tar_target(a_skip,
             c("a_obs_timepoint", "a_cov_type")),


  # get nma dat -------------------------------------------------------------

  # tar_target(
  #   raw_nma_dat,
  #   read_csv("data/nma_dat-2021-09-14 10:24:06.csv")
  # ),


  # temporary dataset -------------------------------------------------------

  tar_target(
    raw_nma_dat,
    read_csv("data/w_obs-2021-09-17.csv")
  ),


  # wrangle nma dat ---------------------------------------------------------


  tar_target(
    nma_dat_wrangle,
    raw_nma_dat %>%
      mutate(across(any_of(
        c("r", "mean", "sd", "se")
      ), as.numeric)) %>%
      mutate(n = as.integer(n))
  ),

  tar_target(
    nma_dat,
    # change back to nma_dat later
    # apply filters
    nma_dat_wrangle %>%
      filter(
        timepoint != "baseline",
        !str_detect(scale, "DELETE"),
        !is.na(intervention),
        type != "unclassified",
        type != "pharmacological intervention",
        type != "non-pharmacological intervention",
        timepoint %in% c("change_score", "post_int"),
        outcome %in% c("pain_sub", "mood", "adverse", "pain_int", "pain_mod")
      )

  ),

  tar_target(
    r_outcome_key,
    read_csv("data/outcome-2021-09-19 16:55:19.csv") %>%
      clean_names()
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
                   forestpath = glue("images/forest/{filename}.png"),
                   pwpath = glue("images/pw/{filename}.png")
                 ) %>%
                 left_join(r_outcome_key, by = c("outcome", "model_type")) %>%
                 select(outcome, everything(), contains("path"))
             }),

  # outcome, timepoint, type ------------------------------------------------


  tar_target(
    m_o_tt_dat,
    nma_dat %>%
      filter(type != "placebo") %>%
      group_by(outcome, timepoint, type) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_o_tt,
    hpp_nma(m_o_tt_dat, nma_dat),
    pattern = map(m_o_tt_dat),
    iteration = "list"
  ),

  tar_target(m_o_tt_key,
             if (!is.null(m_o_tt$error)) {
               tibble(target = "m_o_tt",)
             } else {
               m_key_fn(m_o_tt, "m_o_tt")
             }
             ,
             pattern = map(m_o_tt)),


  # models by condition -----------------------------------------------------


  # condition: pain_sub -----------------------------------------------------



  tar_target(
    m_con_pain_sub_dat,
    nma_dat %>%
      filter(type != "placebo",
             outcome == "pain_sub") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_pain_sub,
    hpp_nma(m_con_pain_sub_dat, nma_dat),
    pattern = map(m_con_pain_sub_dat),
    iteration = "list"
  ),

  tar_target(m_con_pain_sub_key,
             if (!is.null(m_con_pain_sub$error)) {
               tibble(target = "m_con_pain_sub",)
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
    nma_dat %>%
      filter(type != "placebo",
             outcome == "mood") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_mood,
    hpp_nma(m_con_mood_dat, nma_dat),
    pattern = map(m_con_mood_dat),
    iteration = "list"
  ),

  tar_target(m_con_mood_key,
             if (!is.null(m_con_mood$error)) {
               tibble(target = "m_con_mood",)
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
    nma_dat %>%
      filter(type != "placebo",
             outcome == "adverse") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),

  tar_target(
    m_con_adverse,
    hpp_nma(m_con_adverse_dat, nma_dat),
    pattern = map(m_con_adverse_dat),
    iteration = "list"
  ),

  tar_target(m_con_adverse_key,
             if (!is.null(m_con_adverse$error)) {
               tibble(target = "m_con_adverse",)
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

  # list of which interventions are in which studies
  tar_target(pw_study_int,
             nma_dat %>%
               select(type, study, intervention, arm) %>%
               pivot_wider(
                 names_from = intervention,
                 values_from = arm,
                 values_fn = length
               ) %>% group_split(type)
             ),

  # alternate idea
  tar_target(pw_int_summarise,
             nma_dat %>%
               group_by(study) %>%
               summarise(
                 interventions = unique(intervention) %>% paste(collapse = ";")
               )
             ),

  # pw set groups
  tar_target(pw_combn,

             )

  -----------------------------------------------------------


  tar_target(pw_dat_get,
             {
               function(key) {
                 # checks
                 assert_that(
                   key$target == "m_o_tt" | str_detect(key$target, "^m_con"),
                   msg = "If else for mod objects
                           not working as should."
                 )

                 assert_that(length(key$target) == 1,
                             msg = "target identifier not a single string")

                 assert_that(is.character(key$target),
                             msg = "what I think is target string not a string")
                 msg_mine(" Get model input data for")
                 key %>%
                   select(target, index, outcome, timepoint, type) %>%
                   print()

                 msg_mine(" Show target selected, damnit!")
                 pull(key, "target") %>% print()

                 mod <-
                   if (key$target == "m_o_tt")
                     m_o_tt[[key$index]]
                 else if (key$target == "m_con_pain_sub")
                   m_con_pain_sub[[key$index]]
                 else if (key$target == "m_con_mood")
                   m_con_mood[[key$index]]
                 else if (key$target == "m_con_adverse")
                   m_con_adverse[[key$index]]

                 mod %>%
                   pluck("result", "network", "agd_arm") %>%
                   rename(study = .study, intervention = .trt) %>%
                   mutate(across(c(study, intervention), as.character))
               }
             }),

  tar_target(
    pw_dat,
    pw_dat_get(m_key),
    pattern = map(m_key),
    iteration = "list"
  ),

  tar_target(
    pw_nma_comp,
    {
      combn_fct <-
        pw_dat %>%
        pull(intervention) %>%
        unique() %>%
        combn(2)

      int_comb <-
        tibble(g1 = combn_fct[1, ] %>% as.character(),
               g2 = combn_fct[2, ] %>% as.character()) %>%
        mutate(
          studies = map2(g1, g2, find_pw_studies, pw_dat),
          n_studies = map_int(studies, length)
        ) %>%
        filter(n_studies > 1)

      list(m_key = m_key,
           int_comb = int_comb,
           dat = pw_dat)
    },
    pattern = map(pw_dat, m_key),
    iteration = "list"
  ),

  # pw meta-analyse ---------------------------------------------------------


  tar_target(
    pw_ma,
    {
      msg_mine("combinations selected")

      pw_nma_comp$int_comb %>% print()

      if (nrow(pw_nma_comp$int_comb) == 0) {
        msg_mine("only one study in this nma pw comparison")
        "no studies with both interventions"
      } else {
        msg_mine("get studies")
        ma_dat <-
          pw_nma_comp$int_comb %>%
          select(-n_studies) %>%
          pmap(function(g1, g2, studies) {
            this_ma_dat <-
              pw_nma_comp$dat %>%
              filter(intervention %in% c(g1, g2),
                     study %in% studies)

            m_type <- this_ma_dat %>% pull(model_type) %>% unique()

            msg_mine("Create ma dataset")

            mod_dat <-
              this_ma_dat %>%
              filter(intervention == g1) %>%
              select(study, intervention, mean, r, sd, n, arm) %>%
              rename_with(~ glue("{.x}_g1"),
                          any_of(c(
                            "intervention", "arm", "mean", "r", "sd", "n"
                          ))) %>%
              full_join(this_ma_dat %>%
                          filter(intervention == g2) %>%
                          rename_with(~ glue("{.x}_g2"),
                                      any_of(
                                        c("intervention",
                                          "arm",
                                          "mean", "r", "sd", "n")
                                      )),
                        by = "study")

            msg_mine(glue("Meta-analyse: {m_type}"))

            this_escalc <-
              if (m_type == "lor") {
                escalc(
                  data = mod_dat,
                  ai = r_g1,
                  ci = r_g2,
                  n1i = n_g1,
                  n2i = n_g2,
                  measure = "OR",
                  slab = study
                )
              } else if (m_type == "smd") {
                escalc(
                  data = mod_dat,
                  m1i = mean_g1,
                  m2i = mean_g2,
                  sd1i = sd_g1,
                  sd2i = sd_g2,
                  n1i = n_g1,
                  n2i = n_g2,
                  measure = "SMD",
                  slab = study
                )
              } else {
                msg_mine("escalc didn't run")
              }

            rma(
              yi = yi,
              vi = vi,
              data = this_escalc,
              slab = study,
              measure = if_else(m_type == "lor", "OR", "SMD")
            )

          }) -> ma


        c(pw_nma_comp, ma = list(ma))
      }
    },
    pattern = map(pw_nma_comp),
    iteration = "list"
  ),

  tar_target(pw_results, {
    if (length(pw_ma) == 1) {
      pw_nma_comp$m_key %>%
        select(outcome, timepoint, type, condition, index)
    } else {
      pw_ma$int_comb %>%
        mutate(
          i_sq = map_dbl(pw_ma$ma, "I2"),
          tau_sq = map_dbl(pw_ma$ma, "tau2"),
          ci_lb = map_dbl(pw_ma$ma, "ci.lb"),
          ci_ub = map_dbl(pw_ma$ma, "ci.ub"),
          eff_est = map_dbl(pw_ma$ma, "beta")
        ) %>%
        cbind(pw_ma$m_key %>% select(outcome, timepoint, type, condition, index, target))
    }
  },
  pattern = map(pw_ma, pw_nma_comp)),



  # pw tab ------------------------------------------------------------------
  tar_target(pw_tab, {
    # this pw
    m_key %>%
      select(outcome, type, timepoint, condition, target) %>%
      inner_join(pw_results,
                 by = c("outcome", "type", "timepoint", "condition", "target"))

  },
  pattern = map(m_key),
  iteration = "list"),

  # plot ma -----------------------------------------------------------------
  # tar_target(pw_plot, {
  #   msg_mine("Plot target for")
  #   pw_ma$m_key %>% print()
  #
  #   fname <- pw_ma %>% pluck("m_key", "filename")
  #   msg_mine("Filename for these ma")
  #   fname %>% print()
  #
  #   msg_mine("Make plots")
  #   paths <-
  #     map_chr(1:length(pw_ma$ma), function(ma_index) {
  #       glue("images/ma/{fname}-{ma_index}")
  #     })
  #
  #   map2(pw_ma$ma, paths, function(ma, path) {
  #     path
  #
  #   })
  # },
  # pattern = map(pw_ma)),
  #

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
