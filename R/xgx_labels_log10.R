#' Nice labels for log10.
#'
#' Returns a set of labels for ggplot
#'
#' @param breaks, breaks for the function
#'
#' @return either character or expression
#'
#' @examples
#' print(xgx_labels_log10(c(1e-5, 1, 1e5)))
#' 
#' @export
xgx_labels_log10 <- function(breaks) {
  labels <- as.character(breaks)
  if (all(log10(breaks) == as.integer(log10(breaks)), na.rm = TRUE))
     if (min(breaks, na.rm = TRUE) < 0.001 || max(breaks, na.rm = TRUE) > 9999)
       labels <- as.character(breaks)
  return(labels)
}
