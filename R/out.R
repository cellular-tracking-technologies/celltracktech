#' Out
#'
#' @param x - dataframe
#' @param contents
#' @param timezone
#'
#' @returns newtimecol
#' @export
#'
#' @examples
out <- function(x,
                contents,
                timezone) {
  x <- which(names(contents) == x)
  timecol <- contents[, x]
  if (is.character(timecol)) {
    DatePattern <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?"
    exactDatePattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[T, ][[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}(.[[:digit:]]{3})?[Z]?$"
    brokenrow <- grep(exactDatePattern, timecol, invert = TRUE) # find row that has a date embedded in a messed up string (i.e. interrupted rows)
    timecol[brokenrow] <- substring(timecol[brokenrow], regexpr(DatePattern, timecol[brokenrow]))
    timecol[brokenrow[which(regexpr(DatePattern, timecol[brokenrow]) < 0)]] <- NA
    newtimecol <- as.POSIXct(timecol, tz = timezone)
  } else {
    newtimecol <- timecol
  }
  return(newtimecol)
}
