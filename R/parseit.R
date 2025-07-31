
#' Function to parse Payload and extract temperature and battery voltage
#'
#' @param y - hexstring
#'
#' @returns vector - solar battery voltage, temperature in C
#' @export
#'
#' @examples
#' parsed <- t(sapply(blu[[payload_column]], parseit))

parseit <- function(y) {
  hex_to_raw <- function(x) {
    digits <- strtoi(strsplit(x, "")[[1]], base=16L)
    raw <- as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
    readBin(raw, what = 'integer', n = 1, size = 2)
  }

  tryCatch({
    if (nchar(y) > 3) {
      vals <- sapply(seq(from = 1, to = nchar(y), by = 4), function(i) substr(y, i, i + 3))
      parsedvals <- sapply(vals, hex_to_raw)

      # Assuming parsedvals[1] is battery voltage (in millivolts) and parsedvals[2] is temperature (in millidegrees)
      battery_voltage <- parsedvals[1] / 1000 # Convert millivolts to volts
      temperature <- parsedvals[2] / 100 # Convert millidegrees to degrees Celsius

      # return(c(temperature, battery_voltage))
      return(c(battery_voltage, temperature))
    } else {
      return(c(NA, NA))
    }
  }, error = function(msg) { return(c(NA, NA)) })
}
