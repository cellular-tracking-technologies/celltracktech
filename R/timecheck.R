timecheck <- function(contents, myrowfix) {
  time <- ifelse(is_posixct(contents[, 1][[1]]), as.POSIXct(myrowfix[1], tz = "UTC"), myrowfix[1])
  return(time)
}
