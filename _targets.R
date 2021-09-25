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
  # raw data ----------------------------------------------------------------

  tar_target(
    r_obs_dat,
    read_csv("data/obs_dat-2021-09-25 09:12:28.csv") %>%
      clean_names()
  ),

  tar_target(
    r_outcome_key,
    read_csv("data/outcome-2021-09-25 08:56:22.csv") %>%
      clean_names()
  ),


  # temporary dataset -------------------------------------------------------

  # tar_target(r_obs_dat,
  #            read_csv("data/w_obs-2021-09-17.csv")),


  # wrangle obs dat ---------------------------------------------------------

  tar_target(w_obs_dat,
             r_obs_dat %>%
               mutate(across(
                 where(is.character), tolower
               ))),


  # all data wrangled -------------------------------------------------------

  tar_target(obs_dat,
             w_obs_dat),

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

  # get interventions per study
  tar_target(
    pw_study_int,
    obs_dat %>%
      group_by(outcome, type, study) %>%
      summarise(interventions = unique(intervention)
                %>% paste(collapse = ";")) %>%
      mutate(int_list = str_split(interventions, ";")) %>%
      arrange(study)

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
