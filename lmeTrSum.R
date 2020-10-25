lmeTrSum <- function(fixed, data, random, ...) {
  # Returns a list with elements named "treatment" and "sum".
  # The "treatment" element is the nlme::lme object fitted using treatment contrasts.
  # The "sum" element is the nlme::lme object fitted using sum contrasts.
  # The "fixed", "data" and "random" arguments have the same meaning as in lme().
  contr.labels <- c("treatment", "sum")
  lmeTS <- vector("list", length(contr.labels))
  names(lmeTS) <- contr.labels
  #vars.fixed <- all.vars(fixed)[-1]  # [-1] removes the response variable.
  vars.fixed <- intersect(all.vars(fixed)[-1], names(data))
  # [-1] removes the response variable.
  # Intersect with names(data) to avoid trouble with a named constant like "pi"
  # being included, as in a formula like
  # `follicles ~ sin(2*pi*Time) + cos(2*pi*Time)`, which is used on p. 240 of
  # the Pinheiro & Bates book. There is still the possibility of an error if
  # `data` also contains a factor variable called "pi", but having this as well
  # as the constant "pi" in the formula would make the formula ambiguous and
  # would be extremely rare.
  vars.fixed.factor <- vars.fixed[sapply(data[, vars.fixed], class) == "factor"]
  for (contr.lab in contr.labels) {
    if (length(vars.fixed.factor) > 0) {
      contrasts.list <- as.list(rep(paste0("contr.", contr.lab),
                                    length(vars.fixed.factor)))
      names(contrasts.list) <- vars.fixed.factor
    }
    else {
      contrasts.list <- NULL
    }
    lmeTS[[contr.lab]] <- lme(fixed = fixed, data = data, random = random,
                              contrasts = contrasts.list, ...)
  }
  lmeTS
}
