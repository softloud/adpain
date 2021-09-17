{
library(googlesheets4)
library(here)
library(glue)
library(readr)

label_gs <- "https://docs.google.com/spreadsheets/d/1Cy5Vk42ZbeoLmZwwqdwVH0KwVpEStCTvdk1WT_m_yTQ/edit#gid=0"
label_path <- "data/labels"

get_atm <- function(){
  lubridate::now()
}

}

# class -------------------------------------------------------------------

{
atm <- get_atm()  
  
system("rm data/labels/class*.csv")

class_labels <- read_sheet(label_gs, "class")

write_csv(class_labels, here(label_path, glue("class-{atm}.csv")))
}

# outcomes ----------------------------------------------------------------
{
atm <- get_atm()  
  
system("rm data/labels/outcome*.csv")

outcome_labels <- read_sheet(label_gs, "outcome")

write_csv(outcome_labels, here(label_path, glue("outcome-{atm}.csv")))
}

# condition ---------------------------------------------------------------
{
  atm <- get_atm()  
  
system("rm data/labels/condition*.csv")

condition_labels <- read_sheet(label_gs, "condition")

write_csv(condition_labels, here(label_path, glue("condition-{atm}.csv")))
}

# scale unmatched ---------------------------------------------------------

system("rm data/labels/scale_unmatched*.csv")

scale_unmatched_labels <- read_sheet(label_gs, "scales-unmatched-09-06")

write_csv(scale_unmatched_labels, here(label_path, glue("scale_unmatched-{atm}.csv")))


# type --------------------------------------------------------------------

type_gs <- "https://docs.google.com/spreadsheets/d/1D4ccwbILjqxk3QPZdq2p4HdvkKBkfJVETXCQ2XhuNe4/edit#gid=1815379416"

system("rm data/labels/type*.csv")

type_labels <- read_sheet(type_gs, "type", col_types = "c")

write_csv(type_labels, here(label_path, glue("type-{atm}.csv")))


# intervention non ad -----------------------------------------------------

{
  atm <- get_atm()  
  
  system("rm data/labels/intervention-nonad*.csv")
  
  intervention_nonad_labels <- read_sheet(label_gs, "intervention_nonad")
  
  write_csv(intervention_nonad_labels, here(label_path, glue("intervention-nonad-{atm}.csv")))
}

