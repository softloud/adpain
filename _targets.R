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
  
  tar_target(
    raw_nma_dat,
    read_csv("data/nma_dat-2021-09-14 10:24:06.csv")
  ),
  
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
    # apply filters
    nma_dat_wrangle %>% 
      filter(
        timepoint != "baseline",
        !str_detect(scale, "DELETE"),!is.na(intervention),
        !str_detect(covidence, "change"),
        type != "unclassified",
        outcome %in% c("pain_sub", "pain_mod", "mood", "sleep", "adverse")
      )
  ),
  
  tar_target(
    r_outcome_labels,
    read_csv(
      "data/labels/outcome-2021-09-09 03:37:04.csv",
      col_types = cols(.default = "c")
    ) %>%
      clean_names()
  ),
  
  tar_target(
    m_key,
    tibble(outcome = nma_dat %>% pull(outcome) %>% unique()) %>%
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
  
  
  # models ------------------------------------------------------------------
  
  
  # outcome, timepoint, type ------------------------------------------------
  
  
  tar_target(
    m_o_tt_group,
    nma_dat %>%
      filter(type != "placebo") %>%
      group_by(outcome, timepoint, type) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    m_o_tt_dat,
    m_o_tt_group,
    pattern = map(m_o_tt_group),
    iteration = "list"
  ),
  
  
  tar_target(
    m_o_tt,
    
    {
      placebo_dat <-
        m_o_tt_dat %>%
        select(outcome, study, timepoint) %>%
        distinct() %>%
        inner_join(nma_dat %>%
                     filter(type == "placebo"),
                   by = c('outcome', "study", "timepoint"))
      
      dat <-
        m_o_tt_dat %>%
        bind_rows(placebo_dat) %>%
        viable_observations() %>%
        distinct()
      
      m_type <-
        m_o_tt_dat %>%
        pull(model_type) %>%
        unique()
      
      
      hpp_net(dat, m_type) %>%
        safe_nma(trt_effects = "random")
    },
    pattern = map(m_o_tt_dat),
    iteration = "list"
  ),
  
  tar_target(m_o_tt_key,
             if (!is.null(m_o_tt$error)) {
               tibble(outcome = "nma not produced",)
             } else {
               m_o_tt %>%
                 pluck("result", "network", "agd_arm") %>%
                 filter(type != "placebo") %>%
                 summarise(
                   outcome = unique(outcome),
                   timepoint = unique(timepoint),
                   type = unique(type),
                   model_type = unique(model_type),
                   trt_ref = m_o_tt$result$network$treatments[[1]]
                 ) %>%
                 mutate(target = "m_o_tt") %>%
                 select(target, everything())
             }
             ,
             pattern = map(m_o_tt)),
  
  
  # models by condition -----------------------------------------------------
  
  tar_target(
    m_con_o_tt_group,
    nma_dat %>%
      filter(type != "placebo") %>%
      group_by(outcome, timepoint, type, condition_general) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    m_con_o_tt_dat,
    m_con_o_tt_group,
    pattern = map(m_con_o_tt_group),
    iteration = "list"
  ),
  
  
  tar_target(
    m_con_o_tt,
    
    {
      placebo_dat <-
        m_con_o_tt_dat %>%
        select(outcome, study, timepoint, condition_general) %>%
        distinct() %>%
        inner_join(nma_dat %>%
                     filter(type == "placebo"),
                   by = c('outcome', "study", "timepoint"))
      
      dat <-
        m_con_o_tt_dat %>%
        bind_rows(placebo_dat) %>%
        viable_observations() %>%
        distinct()
      
      m_type <-
        m_con_o_tt_dat %>%
        pull(model_type) %>%
        unique()
      
      
      hpp_net(dat, m_type) %>%
        safe_nma(trt_effects = "random")
    },
    pattern = map(m_con_o_tt_dat),
    iteration = "list"
  ),
  
  tar_target(m_con_o_tt_key,
             if (!is.null(m_con_o_tt$error)) {
               tibble(outcome = "nma not produced",)
             } else {
               m_con_o_tt %>%
                 pluck("result", "network", "agd_arm") %>%
                 filter(type != "placebo") %>%
                 summarise(
                   outcome = unique(outcome),
                   timepoint = unique(timepoint),
                   type = unique(type),
                   condition = unique(condition_general),
                   model_type = unique(model_type),
                   trt_ref = m_con_o_tt$result$network$treatments[[1]]
                 ) %>%
                 mutate(target = "m_con_o_tt") %>%
                 select(target, everything())
             }
             ,
             pattern = map(m_con_o_tt)),
  
  
  # wrangle model keys ------------------------------------------------------
  
  tar_target(
    m_keys_list,
    list(m_o_tt = m_o_tt_key,
         m_con_o_tt = m_con_o_tt_key)
  ),
  
  tar_target(m_keys,
             m_keys_list %>%
               map(function(df) {
                 df %>%
                   mutate(index = row_number()) %>%
                   filter(outcome != "nma not produced") %>%
                   unite(filename, everything(), sep = "-", remove = FALSE) %>%
                   mutate(
                     netpath = glue("images/net/{filename}.png"),
                     forestpath = glue("images/forest/{filename}.png")
                   ) %>%
                   left_join(m_key, by = c("outcome", "model_type")) %>%
                   select(index, outcome, everything(), contains("path"))
               })),
  
  tar_target(m_keys_df,
             bind_rows(m_keys) %>%
               left_join(r_outcome_labels)
               ),
  
  tar_target(m_keys_df_m_o_tt,
             m_keys_df %>%
               filter(target == "m_o_tt")),
  
  # network plots -----------------------------------------------------------
  tar_target(plot_net, {
    msg_mine("Set up outcome, type, timepoint")
    this_title <-
      glue("Direct evidence for {m_keys_df$outcome}")
    
    this_title %>% print()
    
    msg_mine("Target:")
    m_keys_df$target %>% print()
    
    msg_mine("Index:")
    m_keys_df$index %>% print()
    
    msg_mine("Set up target-dependent titles")
    this_subtitle <- case_when(
      m_keys_df$target == "m_o_tt" ~
        glue(
          "Subgroups: timepoint {m_keys_df$timepoint} and type {m_keys_df$type}"
        ),
      m_keys_df$target == "m_con_o_tt" ~
        glue(
          "Subgroups: timepoint {m_keys_df$timepoint}, type {m_keys_df$type}, condition {m_keys_df$condition}"
        )
    )
    
    this_subtitle %>% print()
    
    mod <-
      if (m_keys_df$target == "m_o_tt") {
        m_o_tt %>% pluck(m_keys_df$index)
      } else if (m_keys_df$target == "m_con_o_tt") {
        m_con_o_tt %>%
          pluck(m_keys_df$index)
      }
    
    msg_mine("Create plot")
    mod %>%
      pluck("result", "network") %>%
      plot() %>%
      labs(subtitle = this_subtitle,
           title = this_title)
    
  },
  pattern = map(m_keys_df)),
  
  tar_target(plot_net_write,
             {
               glue(
                 " Writing net plot for m_o_tt:
                        outcome {m_keys_df$outcome}
                        timepoint {m_keys_df$timepoint}
                        type {m_keys_df$type}
                        condition {m_keys_df$condition}"
               ) %>% msg_mine()
               
               msg_mine(m_keys_df$netpath)
               ggsave(
                 here::here("bksite",
                            m_keys_df$netpath),
                 plot_net,
                 width = 11,
                 height = 6
               )
             },
             pattern = map(m_keys_df, plot_net)),
  
  
  # forest ------------------------------------------------------------------
  
  
  # forest: generic ---------------------------------------------------------
  
  # this is a check that forest_multinma works
  tar_target(plot_forest_generic, {
    msg_mine("Select an arbitrary model")

    this_mod <-
      m_o_tt %>%
      pluck(1, "result")
    
    # print(summary(this_mod))
    
    msg_mine("Identify the dataframe req for conf ints text")
    # this_mod
    
    msg_mine("Plot generic forest")
    
    forest_multinma(this_mod)
  }),
  
  
  # forest: dev -------------------------------------------------------------
  tar_target(plot_forest_dev, {
    # select an arbitrary lor model
    m_o_tt_key_row <-
      m_keys_df %>%
      filter(outcome == "pain_sub") %>% 
      slice(1)
    
    st <- glue("")
    
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
      if (m_keys_df$target == "m_o_tt") {
        m_o_tt %>% pluck(m_keys_df$index, "result")
      } else if (m_keys_df$target == "m_con_o_tt") {
        m_con_o_tt %>%
          pluck(m_keys_df$index, "result")
      }
    
    plot <-
    hpp_forest(mod,
               m_keys_df)
    
    ggsave(m_keys_df$forestpath, plot, height = 11, width = 11)
    
  },
  pattern = map(m_keys_df)),
  
  tar_target(
    plot_forest_write,
    m_keys_df %>%
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
  
  
  # pw set groups -----------------------------------------------------------
  
  
  tar_target(pw_dat_get,
             {
               function(key) {
                 # checks
                 assert_that(key$target %in% c("m_o_tt", "m_con_o_tt"),
                             msg = "If else for mod objects
                           not working as should.")
                 
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
                 else if (key$target == "m_con_o_tt")
                   m_con_o_tt[[key$index]]
                 
                 mod %>%
                   pluck("result", "network", "agd_arm") %>%
                   rename(study = .study, intervention = .trt) %>%
                   mutate(across(c(study, intervention), as.character))
               }
             }),
  
  tar_target(pw_dat,
             pw_dat_get(m_keys_df),
             pattern = map(m_keys_df)),
  
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
      
      list(m_key = m_keys_df,
           int_comb = int_comb,
           dat = pw_dat)
    },
    pattern = map(pw_dat, m_keys_df),
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
  
  
  # plot ma -----------------------------------------------------------------
  tar_target(pw_plot, {
    msg_mine("Plot target for")
    pw_ma$m_key %>% print()
    
    fname <- pw_ma %>% pluck("m_key", "filename")
    msg_mine("Filename for these ma")
    fname %>% print()
    
    msg_mine("Make plots")
    list(trees = pw_ma$ma %>%
           map(forest),
         paths =
           map_chr(1:length(pw_ma$ma), function(ma_index) {
             glue("images/ma/{fname}-{ma_index}")
           }))
  },
  pattern = map(pw_ma)),
  
  
  # null --------------------------------------------------------------------
  
  NULL
)
