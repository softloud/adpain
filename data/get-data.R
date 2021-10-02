{
  library(googlesheets4)
  library(here)
  library(glue)
  library(readr)

  obs_dat_gs <- "https://docs.google.com/spreadsheets/d/1CMsqvOC3SteW1FsSomyfSAYSXRspuU17PWNJbrKRZKk/edit#gid=0"

}



# update obs --------------------------------------------------------------

obs_file <- glue("obs_dat-{lubridate::now()}.csv")

dat <- read_sheet(obs_dat_gs, "all data", col_types = "c")

system("rm data/obs_dat*.csv")

here("data", obs_file) %>%
  write_csv(dat, .)

# update outcome labels ---------------------------------------------------

outcome_key <- read_sheet(obs_dat_gs, "outcome", col_types = "c")

outcome_file <- glue("outcome-{lubridate::now()}.csv")

system("rm data/outcome*.csv")

here("data", outcome_file) %>%
  write_csv(outcome_key, .)
