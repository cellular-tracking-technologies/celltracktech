querygen <- function(mycont) {
  pieces <- paste(names(mycont), mycont, sep = " = ")
  na <- grep(" = NA", pieces)
  if (length(na > 0)) {
    pieces[na] <- gsub("= NA", "is null", pieces[na])
  }
  pieces <- paste(pieces, collapse = " and ")
  return(pieces)
}
