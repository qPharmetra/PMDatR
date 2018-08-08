#' @export
`+.pmunits` <- function(e1, e2) {
  ue2 <- units(e2)
  ue1 <- units(e1)
  if (!is.null(ue1) && !is.null(ue2)) {
    if(convertible(ue1, ue2)) {
      e2 <- udunits2::ud.convert(e2, ue2, ue1)
    } else {
      stop("units incompatible")
    }
  }
  NextMethod(e1, e2)
}

#' @export
`-.pmunits` <- function(e1, e2) {
  ue2 <- units(e2)
  ue1 <- units(e1)
  if (!is.null(ue1) && !is.null(ue2)) {
    if(convertible(ue1, ue2)) {
      e2 <- udunits2::ud.convert(e2, ue2, ue1)
    } else {
      stop("units incompatible")
    }
  }
  NextMethod(e1, e2)
}

#' @export
`*.pmunits` <- function(e1, e2) {
  ue2 <- units(e2)
  ue1 <- units(e1)
  if (!is.null(ue1) && !is.null(ue2)) {
    if(grepl("/", ue2) || grepl("/", ue1)) {
      # see /.pmunits for reasoning
      res <- set_units(NextMethod(e1, e2), paste(wrap_parenths(ue1), wrap_parenths(ue2), sep = "*"))
    } else {
      res <- set_units(NextMethod(e1, e2), paste(ue1, ue2, sep = "*"))
    }
  } else {
    res <- NextMethod(e1, e2)
  }
  return(res)
}

wrap_parenths <- function(x) {
  paste0('(', x, ')')
}

#' @export
`/.pmunits` <- function(e1, e2) {
  ue2 <- attr(e2, 'pmunits')
  ue1 <- attr(e1, 'pmunits')
  if (!is.null(ue1) && !is.null(ue2)) {
    if(grepl("/", ue2) || grepl("/", ue1)) {
      # if either units already has a divisor, need to wrap in parens
      # so parsing can happen ok eg.
      # mg/L/g/mol will not parse but (mg/L)/(g/mol) will properly parse
      res <- set_units(NextMethod(e1, e2), paste(wrap_parenths(ue1), wrap_parenths(ue2), sep = "/"))
    } else {
      res <- set_units(NextMethod(e1, e2), paste(ue1, ue2, sep = "/"))
    }
  } else {
    res <- NextMethod(e1, e2)
  }
  return(res)
}

#' @export
`^.pmunits` <- function(e1, n) {

  ue1 <- units(e1)
  if (!is.null(ue1)){ res <- set_units(NextMethod(e1,n), sprintf("(%s)^%g", ue1, n))}
  else res <- NextMethod(e1, n)
  return(res)

}

