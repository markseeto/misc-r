p_round <- function(pvalues, type="2or3dp", na="", lessThanSpace=FALSE) {
  # Returns formatted rounded p-values, keeping trailing zeros.
  # pvalues should be a vector of p-values.
  # type indicates the number of decimal places and should be
  #   "2or3dp" (default), "2dp", "3dp", or "4dp".
  # type = "2or3dp" means 2 decimal places if p >= 0.0095 (p >= 0.01 when rounded),
  #   3 decimal places if 0.001 <= p < 0.0095, and "<0.001" otherwise.
  # type = "2dp", "3dp" or "4dp" means 2, 3 or 4 decimal places, respectively,
  #   with "<0.01", "<0.001" or "<0.0001".
  # na is the string to be used if the p-value is NA.
  # lessThanSpace determines whether a space should appear between the "<"
  #   and the number.
  if (!(type %in% c("2or3dp", "2dp", "3dp", "4dp"))) {
    stop("`type` must be one of \"2or3dp\", \"2dp\", \"3dp\", \"4dp\".")
  }
  if (type != "2or3dp") {
    n.dp <- as.integer(sub("dp", "", type))  # number of decimal places
    lessThanPower <- -n.dp  # number in lessThan will be 10^lessThanPower
  }
  else {
    lessThanPower <- -3L
  }
  lessThan <- paste0("<",
                     ifelse(lessThanSpace, " ", ""),
                     "0.",
                     paste(rep(0, abs(lessThanPower) - 1L), collapse=""),
                     1)
  if (type != "2or3dp") {
    p_rounded <-
      ifelse(is.na(pvalues), na,
             ifelse(pvalues >= 10^lessThanPower,
                    format(round(pvalues, n.dp), nsmall=n.dp),
                    lessThan))
  }
  else {
    p_rounded <-
      ifelse(is.na(pvalues), na,
             ifelse(pvalues >= 0.0095, format(round(pvalues, 2), nsmall=2),
                    ifelse(pvalues >= 0.001, format(round(pvalues, 3), nsmall=3),
                           lessThan)))
  }
  p_rounded
}
