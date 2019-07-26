#' Check data for various issues
#'
#' Calls \code{\link{xgx_check_data}}
#' 
#' @return data.frame
#' 
#' @examples
#' covariates <- c("WEIGHTB", "SEX")
#' check <- xgx_summarize_data(Multiple_Ascending_Dose_Missing_Duplicates,
#'                             covariates)
#' @inheritParams xgx_check_data
#' @export
xgx_summarize_data <- function(data, covariates = NULL) {
  xgx_check_data(data, covariates)
}
