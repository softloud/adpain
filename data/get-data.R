{
  library(googlesheets4)
  library(here)
  library(glue)
  library(readr)
  
  nma_dat_gs <- "https://docs.google.com/spreadsheets/d/1X7pAGFjvZMhtMi3hBqMkbLFjrFsAe4nVMEv8EUs72qw/edit#gid=505181474"
  nma_file <- glue("nma_dat-{lubridate::now()}.csv")  
  
  dat <- read_sheet(nma_dat_gs, "nma_dat", col_types = "c")
  
}

here("data", nma_file) %>% 
  write_csv(dat, .)

