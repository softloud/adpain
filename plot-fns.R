list.files("R", full.names = TRUE) %>%
  map(source)

safe_hpp_nma <- safely(hpp_nma, otherwise = "failed")



get_mod_dat <- function(key_row) {

  dat <-
    if (key_row$subgroup == "subgroup_type") {
      m_type_dat %>%
        pluck(key_row$tar_group)
    } else if (key_row$subgroup == "subgroup_class") {
      m_class_dat %>%
        pluck(key_row$tar_group)
    } else if (key_row$subgroup == "subgroup_dose") {
      m_dose_dat %>%
        pluck(key_row$tar_group)
    } else if (key_row$subgroup == "subgroup_con") {
      m_con_dat %>%
        pluck(key_row$tar_group)
    }

  if (nrow(dat) == 0) stop("No viable obs")

  return(dat)

}



pres_dat <- function(subgroup_row) {
  dat <- get_mod_dat(subgroup_row) %>%
    viable_observations()

}

pres_nma <- function(subgroup_row) {
  mod <-
  pres_dat(subgroup_row) %>% safe_hpp_nma()

  return(mod)

}

write_pres_nma <- function(mod, subgroup_row) {

  path <-
    subgroup_row %>%
    select(subgroup, everything()) %>%
    select(-contains("_"), -contains("label"), -contains("desc"), -studies) %>%
    mutate(out_type = ".rds") %>%
    unite(filepath, everything()) %>%
    here("data", "pres", .)

  msg_mine(glue("Writing file to:

                {path}

                "))

  write_rds(mod, path)

}

pres_filename <- function(filename) {

}

outcome_key <-
  w_outcome_key %>%
  filter(!is.na(direction_of_improvement)) %>%
  filter(!str_detect(outcome, "mood_")) %>%
  select(-outcome) %>%
  rename(outcome = outcome_nma)

key_labels <- function(this_key_row) {
  this_key_row %>%
    left_join(outcome_key) %>%
    left_join(m_timepoint_label_key) %>%
    mutate(
      dose = dose_s,
      condition = condition_s,
      total_participants = pres_dat(this_key_row) %>% pull(n) %>% sum(),
      class = class_s
    ) %>%
    left_join(m_type_label_key)

}


make_forest <- function(
  mod,
  this_key_row,
  this_class = "no class filter",
  width = 10,
  height = 8) {


  row <- this_key_row %>% key_labels() %>%
    mutate(trt_ref = mod$result$network$treatments[[1]]) %>%
    mutate(
      class = if_else(str_detect(class, ";"), "all classes", class),
        dose = if_else(str_detect(dose, ";"), "all doses", dose),
           condition = if_else(str_detect(condition, ";"),
                               "all conditions", condition),
           total_participants = get_mod_dat(this_key_row) %>%
             pluck("n") %>% sum(),
           trt_ref = mod$result$network$treatments[[1]]
    )

  plot <- hpp_forest(mod$result, row,
                     font_size = 15,
                     this_class = this_class
                     )
  return(plot)

}

pres_forest <- function(key_row) {
  # mod <- get_mod(key_row)


  # hpp_forest(mod = mod$result, key_row)

  here(key_row$forestpath) %>%
    read_rds()
}



pres_pw <- function(key_row) {
  pw_forest(key_row)
}

see_pres <- function() {
  browseURL(
    rmarkdown::render("facet-pres.Rmd")
    # xfun::in_dir("bksite", bookdown::render_book("index.Rmd"))
  )

}

