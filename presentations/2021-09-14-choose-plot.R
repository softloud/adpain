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


# functions ---------------------------------------------------------------
list.files("R", full.names = TRUE)  %>%
  map(source)
{
tar_load(m_keys_df)
tar_load(m_o_tt)
tar_load(m_con_o_tt)
tar_load(pw_ma)
}

# display models ----------------------------------------------------------

m_keys_df %>%
  select(outcome, condition, type, timepoint, model_type) %>% 
  gt()

# select result -----------------------------------------------------------

this_model <- m_keys_df %>% 
  mutate(ma_index = row_number()) %>% 
  filter(
    outcome == "sleep",
    is.na(condition)
    # str_detect(condition, "fibro")
  )

mod <-
  if (this_model$target == "m_o_tt") {
    m_o_tt %>% pluck(this_model$index, "result")
  } else if (this_model$target == "m_con_o_tt") {
    m_con_o_tt %>%
      pluck(this_model$index, "result")
  }


# net plot ----------------------------------------------------------------

mod$network %>% plot()

# forest plot -------------------------------------------------------------


  hpp_forest(mod,
             this_model, or = TRUE)


# pairwise ----------------------------------------------------------------
{
pw <- pw_ma[[this_model$ma_index]]

ma <- pw$ma


length(pw)

or <- if (this_model$model_type == "lor") exp else NULL

ma %>% 
  map(forest, transf = or, title = "test")

pw$int_comb

}
