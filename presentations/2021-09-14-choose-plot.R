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

tar_load(m_keys_df)
tar_load(m_o_tt)
tar_load(m_con_o_tt)

# select result -----------------------------------------------------------

m_keys_df %>%
  select(outcome, condition, type, timepoint, model_type) %>% 
  gt()

this_model <- m_keys_df %>% 
  filter(
    outcome == "adverse",
    condition == "neuropathic"
  )

mod <-
  if (this_model$target == "m_o_tt") {
    m_o_tt %>% pluck(this_model$index, "result")
  } else if (this_model$target == "m_con_o_tt") {
    m_con_o_tt %>%
      pluck(this_model$index, "result")
  }



  hpp_forest(mod,
             this_model)


