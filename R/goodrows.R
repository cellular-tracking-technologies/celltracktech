goodrows <- function(rowlen, rowfix, e, correct, DatePattern, filetype) {
  if (length(rowfix) < 2) {
    fixed <- fixrow(rowlen, rowfix, e, correct, DatePattern, filetype)
  } else {
    fixed <- Map(fixrow, rowlen, rowfix, MoreArgs = list(e = e, DatePattern = DatePattern, correct = correct, filetype = filetype))
    fixed <- data.table::rbindlist(fixed, use.names = FALSE)
  }
  return(fixed)
}
