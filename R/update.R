#' Update observation data
#'
#' Update [mod_dat] dataset.
#'
#' @return
#' @export
#'
#' @examples

update_obs <- function() {
  obs_file <- glue("obs_dat-{lubridate::now()}.csv")

  dat <-
    Sys.getenv("obs_dat_gs") %>%
    googlesheets4::read_sheet("NMA analysis dataset",
                              col_types = "c")

  system("rm data-raw/obs_dat*.csv")

  obs_path <-
    glue("data-raw/{obs_file}")

  obs_path %>%
    readr::write_csv(dat, .)

  glue::glue("Data written to:

             {obs_path}

             Don't forget to update read raw target :)") %>%
    dontpanic::msg()
}


#' Title
#'
#' @return
#' @export
#'
#' @examples

update_outcome <- function() {
  # update outcome labels ---------------------------------------------------

  key_file <- googlesheets4::read_sheet(Sys.getenv("obs_dat_gs"), "outcome",
                                           col_types = "c")

  filename <- glue::glue("outcome-{lubridate::now()}.csv")

  system("rm data-raw/outcome*.csv")


  filepath <-
  glue("data-raw/{filename}")

  readr::write_csv(key_file, filepath)

  glue::glue("Data written to:

             {filepath}

             Don't forget to update read raw target :)") %>%
    dontpanic::msg()

}

#' Title
#'
#' @return
#' @export
#'
#' @examples

update_scale <- function() {
  key_file <-
    googlesheets4::read_sheet(Sys.getenv("obs_dat_gs"), "scales", col_types = "c")

  filename <- glue::glue("scales-{lubridate::now()}.csv")

  system("rm data-raw/scales*.csv")

  filepath <-
    glue("data-raw/{filename}")

  readr::write_csv(key_file, filepath)

  glue::glue("Data written to:

             {filepath}

             Don't forget to update read raw target :)") %>%
    dontpanic::msg()


}
