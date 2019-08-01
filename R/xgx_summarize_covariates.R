#'Summarize Covariate information in a dataset
#'
#' \code{xgx_summarize_covariates} 
#'
#' @param data, the dataset to check. must contain a USUBJID or ID column 
#' for subject id
#' @param covariates, the column names of covariates, to explore
#' @param n_cts, the number of unique values for a covariate to be treated as 
#' continuous, default is 8
#'
#' @return list
#'
#' @examples
#' data <- data.frame(ID = 1:10, WT0 = rnorm(10, 70, 10),
#'                    SEX = round(runif(10)))
#' x <- xgx_summarize_covariates(data, c("WT0", "SEX"))
#' 
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom magrittr "%>%"
#' @export
xgx_summarize_covariates <- function(data, covariates = NULL, n_cts = 8) {
  # avoid CRAN note
  ID <- USUBJID <- n <- NULL

  if ("USUBJID" %in% names(data)) {
    data1 <- dplyr::filter(data, !duplicated(USUBJID))
  } else if ("ID" %in% names(data)) {
    data1 <- dplyr::filter(data, !duplicated(ID))
  } else {
    stop("data column USUBJID or ID is required")
  }

  icat <- 0
  icts <- 0
  catlist <- list()
  ctslist <- list()
  for (covk in covariates) {
    x <- data1[[covk]]

    xdistinct <- length(unique(x))
    xmissing <- sum(is.na(x))
    if (xdistinct >= n_cts) {
      icts <- icts + 1
      ctslist[[icts]] <- tibble::tibble(
        Covariate = covk,
        Nmissing = xmissing,
        min = min(x, na.rm = TRUE),
        `25th` = stats::quantile(x, 0.25, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        `75th` = stats::quantile(x, 0.75, na.rm = TRUE),
        max = max(x, na.rm = TRUE))
    } else {
      summ <- tibble::tibble(var = x) %>%
        dplyr::group_by(var) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(n))

      icat <- icat + 1
      catlist[[icat]] <- tibble::tibble(
        Covariate = covk,
        Nmissing = xmissing,
        Ndistinct = xdistinct,
        `Value (Count)` = paste0(summ$var, " (", summ$n, ")", collapse = ", "))
    }
  }

  # create summaries
  cat_table <- dplyr::bind_rows(catlist)
  cts_table <- dplyr::bind_rows(ctslist)

  return(list(cts_covariates = cts_table,
              cat_covariates = cat_table))
}
