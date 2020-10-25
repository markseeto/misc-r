makelist <- function(lnames) {
  # Returns a list (possibly nested) with names given by lnames.
  # lnames should be a vector of names or a list with each element of the list
  # being a vector of names.
  # If lnames is a list with more than one element, lists with names given
  # by later elements will be nested within lists with names given by the
  # previous element.
  # For example, makelist(list(c("a1", "a2"), c("b1", "b2", "b3"))) will be
  # a list with 2 elements, named "a1" and "a2", and each of those two elements
  # will be a list with 3 elements, named "b1", "b2" and "b3".
  if (typeof(lnames) != "list") {
    lnames <- list(as.character(lnames))
  }
  L <- vector("list", length(lnames[[1]]))
  names(L) <- lnames[[1]]
  if (length(lnames) > 1) {
    for (k in 1:length(L)) {
      L[[k]] <- makelist(lnames[-1])
    }
  }
  L
}
