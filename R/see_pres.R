#' Title
#'
#' @return
#' @export
#'
#' @examples

see_pres <- function() {
  browseURL(

    rmarkdown::render(
      "presentations/2021-12-02/pres.Rmd"
      # "sof.Rmd"
      # "pres-2021-11-04.Rmd"
      )
    # xfun::in_dir("bksite", bookdown::render_book("index.Rmd"))
  )

}

#' Preview report in browser
#'
#' @return
#' @export
#'
#' @examples

see_rep <- function() {
  # browseURL(
  #   xfun::in_dir("bksite", bookdown::render_book("index.Rmd"))
  # )

  bookdown::render_book("report", output_format = "pdf_document")
}
