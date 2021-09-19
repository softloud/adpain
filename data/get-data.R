{
  library(googlesheets4)
  library(here)
  library(glue)
  library(readr)

}

  nma_dat_gs <- "https://docs.google.com/spreadsheets/d/1CMsqvOC3SteW1FsSomyfSAYSXRspuU17PWNJbrKRZKk/edit#gid=0"
  nma_file <- glue("nma_dat-{lubridate::now()}.csv")

  dat <- read_sheet(nma_dat_gs, "dat", col_types = "c")

  outcome_key <- read_sheet(nma_dat_gs, "outcome", col_types = "c")

  outcome_file <- glue("outcome-{lubridate::now()}.csv")



here("data", nma_file) %>%
  write_csv(dat, .)


here("data", outcome_file) %>%
  write_csv(outcome_key, .)
