xgx_1_x <- NULL

#' A dummy translation function to capture the units of the x-axis
#'
#' @param set_unit When specified, this will save the unit information
#'   for x-axis transformation.
#' 
#'@export
xgx_1_unit <- function(set_unit){
  if (!missing(set_unit)) {
    utils::assignInMyNamespace("xgx_1_x", set_unit)
    return(set_unit)
  }
  trans <- function(x) {
    if (inherits(x, "units")) {
      xgxr::xgx_1_unit(attr(x, "units"))
      return(x)
    } else {
      return(x)
    }
  }
  inv <- function(x){
    if (inherits(x, "units")) {
      xgxr::xgx_1_unit(attr(x, "units"))
      return(x)
    } else {
      return(x)
    }
  }
  scales::trans_new("unit-1-x", trans, inv)
}

#' xgx log trans for units
#'
#' This takes units and transforms them for a log10 axes
#'
#' @inheritParams scales::log_trans
#' @export
xgx_log_trans_unit <- function (base = exp(1)) {
  force(base)
  trans <- function(x) {
    if (inherits(x, "units")) {
      units <- class(x)
      tmp <- x
      class(tmp) <- NULL
      tmp <- log(tmp, base)
      class(tmp) <- units
      return(tmp)
    } else {
      log(x, base)
    }
  }
  inv <- function(x){
    if (inherits(x, "units")) {
      tmp <- x
      units <- class(x)
      class(tmp) <- NULL
      tmp <- base^(tmp)
      class(tmp) <- units
      return(tmp)
    } else {
      base^(x)
    }
  }
  scales::trans_new(paste0("log-", format(base), "-units"), trans,
                    inv, scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}
#' @rdname xgx_log_trans_units
#' @export
xgx_log10_trans_unit <- function() {
  xgx_log_trans_unit(10)
}
