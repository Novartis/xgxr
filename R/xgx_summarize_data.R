#' Check data for various issues
#'
#' Calls \code{\link{xgx_check_data}}
#' 
#' @inheritParams xgx_check_data
#' @export
xgx_summarize_data = function(data,covariates = NULL){
  xgx_check_data(data,covariates,...)
}
