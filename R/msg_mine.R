#' Charles' custom messages
#'
#' @param msg Message
#'
#' @return
#' @export
#'
#' @examples

msg_mine <- function(msg) {
  message(crayon::blue(msg))
}