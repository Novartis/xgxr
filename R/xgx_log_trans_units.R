
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
