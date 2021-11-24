withr::with_dir(here::here(), {
  tar_load(m_key)
  tar_load(w_obs_outcome)
  tar_load(obs_excluded)
  tar_load(obs_dat)
  tar_load(pw_results)
  tar_load(mod_dat)
  tar_load(adpain)
  tar_load(oti_pw)
  tar_load(oti_results)
  tar_load(oti_sof)
  tar_load(o_nma_key)


  source("R/hpp_themes.R")
  source("R/msg_mine.R")
})

library(knitr)

conflicted::conflict_prefer("filter", "dplyr")

#  copy files to docs -----------------------------------------------------


system("cp -rf images ../docs/")
