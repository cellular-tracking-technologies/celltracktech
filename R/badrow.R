badrow <- function(e, contents, filetype) {
  if (filetype == "raw") {
    correct <- ifelse(ncol(contents) > 5, 6, 5)
  } else if (filetype == "node_health") {
    correct <- ifelse(ncol(contents) > 9, ifelse(ncol(contents) > 13, 19, 13), 6)
  } else if (filetype == "gps") {
    indx <- count.fields(e, sep = ",")
    correct <- ifelse(any(indx == 9), 9, 6)
  }

  file_err <- 0
  indx <- count.fields(e, sep = ",")
  indx[which(is.na(indx))] <- correct
  if (any(indx > correct)) {
    rowfix <- which(indx != correct) - 1
    rowlen <- indx[which(indx != correct)]
    # if (filetype == "gps") {
    #  if(correct == 6 & rowlen == 9) {contents <- contents[rowfix,]}
    # } else {
    file_err <- 3 # what if this is more than 1 row?
    contents <- contents[-rowfix, ]
    fixed <- goodrows(rowlen, rowfix, e, correct, DatePattern, filetype)
    names(fixed) <- names(contents)
    contents <- rbind(contents, fixed)
    # }
  } else if (any(indx < correct)) {
    file_err <- 4
    rowfix <- which(indx != correct) - 1
    rowlen <- indx[which(indx != correct)] # what if this is more than 1 row?
    message(contents[rowfix, ])
    if (filetype == "gps" & correct == 9) {
      if (min(rowfix) < 1) {
        rowfix <- which(indx == 9) - 1
        rowlen <- indx[which(indx == correct)]
        contents <- goodrows(rowlen, rowfix, e, correct, DatePattern, filetype)
      }
    }
    if (!is_posixct(contents[rowfix, 1][[1]])) { # does this matter about how it's read in too? look for why it sometimes doesn't cast that way
      # print(contents[rowfix,])
      contents <- contents[-rowfix, ]
    }
    # else if(length(rowfix) < 2) {
    # datetest <- tryCatch({
    #  is_posixct(contents[rowfix,1]$Time)
    # }, error = function(cond) {
    #  NA
    # })
  } # else {file_err <- 5}
  if (any(indx < correct) & any(indx > correct)) {
    file_err <- 5
  }
  return(list(contents, file_err))
}
