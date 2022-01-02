nma_url <- "https://www.acpjournals.org/na101/home/literatum/publisher/acp/journals/content/aim/2015/aim.2015.162.issue-11/m14-2385/20211001/images/large/6tt1_table_checklist_of_items_to_include_when_reporting_a_systematic_review_involving_a_network.jpeg"

library(rvest)
library(tidyverse)

raw_tab <-
  nma_url %>%
  read_html() %>%
  html_table(header = TRUE) %>%
  pluck(1)
