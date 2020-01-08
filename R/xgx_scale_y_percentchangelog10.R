#' percentchangelog10 transform for the y scale.  
#' 
#' \code{xgx_scale_y_percentchangelog10} is designed to be used with 
#' percent change from baseline data (on a scale of -1 to +Inf)
#' Common examples include % weight loss, % reduction in LDL, % change in tumor diameter.  
#' It is used when you have a wide range of data on a percent change scale, 
#' especially data close to -100%, and/or several fold increase from baseline.
#' 
#' @param breaks if NULL, then default is to use xgx_breaks_log10(x + 1) - 1
#' @param minor_breaks if NULL, then defauls is to use nicely spaced log(PCHG + 1) minor breaks
#' @param labels  if NULL, then the default is to use scales::percent()
#' @param accuracy  if NULL, then use the the default as specified by scales::percent()
#' to round to the hundredths place, set accuracy 0.01
#' @param ... other parameters passed to 
#' \code{\link[ggplot2:scale_continuous]{scale_y_continuous}}
#' 
#' @return ggplot2 compatible scale object
#' 
#' @examples  
#'   
#' Nsubj <- 10
#' Doses <- c(0, 25, 50, 100, 200)
#' Ntot <- Nsubj*length(Doses)
#' dat1 <- data.frame(ID = 1:(Ntot), 
#'                   DOSE = rep(Doses, Nsubj),
#'                   PD0 = rlnorm(Ntot, log(100), 1),
#'                   Kout = exp(rnorm(Ntot,-2, 0.3)),
#'                   Imax = 1,
#'                   ED50 = 25) %>% 
#'  dplyr::mutate(PDSS = PD0*(1 - Imax*DOSE/(DOSE + ED50))*exp(rnorm(Ntot, 0.05, 0.3))  ) %>%
#'  dplyr::mutate(PCHG = (PDSS - PD0)/PD0)
#'
#' ggplot2::ggplot(dat1, ggplot2::aes(x = DOSE, y = PCHG, group = DOSE)) +
#'   ggplot2::geom_boxplot() +
#'   xgx_theme() + 
#'   xgx_scale_y_percentchangelog10()
#'
#' dat2 <- data.frame(ID = 1:(Ntot), 
#'                   DOSE = rep(Doses, Nsubj),
#'                   PD0 = rlnorm(Ntot, log(100), 1),
#'                   Kout = exp(rnorm(Ntot,-2, 0.3)),
#'                   Emax = 50*rlnorm(Ntot, 0, 0.3),
#'                   ED50 = 300) %>% 
#'  dplyr::mutate(PDSS = PD0*(1 + Emax*DOSE/(DOSE + ED50))*exp(rnorm(Ntot, -1, 0.3))  ) %>%
#'  dplyr::mutate(PCHG = (PDSS - PD0)/PD0)
#'
#' ggplot2::ggplot(dat2, ggplot2::aes(x = DOSE, y = PCHG, group = DOSE)) +
#'   ggplot2::geom_boxplot() +
#'   xgx_theme() + 
#'   xgx_scale_y_percentchangelog10()
#'
#'   
#'  
#' @importFrom scales trans_new
#' @importFrom scales percent_format
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom dplyr mutate
#' @export
xgx_scale_y_percentchangelog10 <- function(breaks = NULL,
                                           minor_breaks = NULL,
                                           labels = NULL, 
                                           accuracy = NULL, 
                                           ...) {
  
  if (is.null(breaks)){
    breaks <- function(x) xgx_breaks_log10(x + 1) - 1
  }
  
  if (is.null(minor_breaks)) {
    minor_breaks <-  function(x) xgx_minor_breaks_log10(x + 1) - 1
  }
  
  percentchangelog <- scales::trans_new(
    name      = "percentchangelog",
    transform = function(x) log10(x + 1),
    inverse = function(x) 10^(x) - 1,
    breaks = breaks)
  
  if (is.null(labels)) {
    labels = scales::percent_format(accuracy = accuracy)
  }
  
  ggplot2::scale_y_continuous(trans = percentchangelog,
                              labels = labels,
                              minor_breaks = minor_breaks, ...)
}
