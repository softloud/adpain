library(rvest)
library(tidyverse)

mecir_url <- "https://community.cochrane.org/mecir-manual/standards-reporting-new-cochrane-intervention-reviews-r1-r109/results-r56-r109/effects-interventions-r76-r99"

raw_tab <-
  mecir_url %>%
  read_html() %>%
  html_table(header = TRUE) %>%
  pluck(1)


tab <-
  raw_tab %>%
  as.data.frame() %>%
  rename(id = "") %>%
  janitor::clean_names() %>%
  mutate(
    id = ifelse(
      id == "",
      NA,
      id
    ),
    id_key = if_else(is.na(id), "details", "title")
  ) %>%
fill(id, .direction = "down")


mecir_wide <-
tab %>%
  pivot_wider(
    id_cols = id,
    values_from = c(standard, rationale_and_elaboration),
    names_from = id_key,
    names_glue = "{.value}_{id_key}"
  ) %>%
  left_join(tab %>%
              filter(str_length(resources) > 2) %>%
              select(id, resources)) %>%
  mutate(across(everything(), str_remove_all, pattern = "\\n|\\t"))


