timeset <- function(g) {
  unname(sapply(g, function(h) ifelse(is.na(h), NA, paste(as.character(h), "UTC"))))
}
