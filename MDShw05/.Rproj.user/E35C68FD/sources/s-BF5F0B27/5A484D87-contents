#' MTA NYC subway delay announcement
#'
#' Allows one to report train delay in New York City subways. It will work for any of the 23 MTA lines in New York.
#' @param line_no Subway line number.
#' @param delay_time Expected time delay in minutes.
#' @import lubridate
#' @export
#' @examples
#' delay_ann()
#' delay_ann("1", 5)
#' delay_ann("C", 10)

delay_ann <- function(line_no = "0", delay_time = 0){
  eta <- format(Sys.time() + minutes(delay_time), "%H:%M")
  mta_lines <- c("0", "1", "2", "3", "4", "5", "6", "7", "A", "B", "C", "D", "E", "F", "G", "J", "L", "M", "N", "Q", "R", "S", "Z", "SIR")
  if (is.element(line_no, mta_lines) == FALSE | is.numeric(delay_time) == FALSE) {
    return("Please enter a valid MTA line and/or a numeric delay time. input line_no = 0 and delay_time = 0 if there is no delay")
  } else {
    if (line_no == "0") {
      if (delay_time == 0) {
        cat("There is no train delay.")
      } else {
          cat("Please enter a valid MTA line and/or a numeric delay time. Input line_no = 0 and delay_time = 0 if there is no delay")
      }
    } else {
      if (delay_time == 0) {
        cat("Please input a delay_time value > 0")
      } else {
        cat("Attention please: Line", {line_no}, "is experiencing a delay of", {delay_time}, "minute(s).", "The estimated time of arrival is", {eta},"HRS. We are sorry for the inconvenience.")
      }
    }
  }
}
