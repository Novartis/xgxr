#' Plot Emax fit to data
#' Uses minpack.lm::nlsLM, predictdf.nls, and stat_smooth to display Emax model fit to data
#' 
#' @section Warning:
#' \code{nlsLM} uses \code{nls.lm} which implements the Levenberg-Marquardt
#' algorithm for fitting a nonlinear model, and may fail to converge for a
#' number of reasons. See \code{?nls.lm} for more information.
#' 
#' \code{nls} uses Gauss-Newton method for estimating parameters, 
#' and could fail if the parameters are not identifiable. If this happens 
#' you will see the following warning message: 
#' Warning message:
#' Computation failed in `stat_smooth()`:
#'   singular gradient
#'   
#' \code{nls} will also fail if used on artificial "zero-residual" data, 
#' use \code{nlsLM} instead.
#'
#' @inheritParams ggplot2::geom_smooth
#'
#' @examples 
#' Nsubj <- 10
#' Doses <- c(0, 25, 50, 100, 200)
#' Ntot <- Nsubj*length(Doses)
#' times <- c(0,14,30,60,90)
#' 
#' dat1 <- data.frame(ID = 1:(Ntot),
#'                    DOSE = rep(Doses, Nsubj),
#'                    PD0 = rlnorm(Ntot, log(100), 1),
#'                    Kout = exp(rnorm(Ntot,-2, 0.3)),
#'                    Imax = 1,
#'                    ED50 = 25) %>%
#'   dplyr::mutate(PDSS = PD0*(1 - Imax*DOSE/(DOSE + ED50))*exp(rnorm(Ntot, 0.05, 0.3))  ) %>%
#'   merge(data.frame(ID = rep(1:(Ntot), each = length(times)), Time = times), by = "ID") %>%
#'   dplyr::mutate(PD = ((PD0 - PDSS)*(exp(-Kout*Time)) + PDSS), 
#'                 PCHG = (PD - PD0)/PD0)
#' 
#' gg <- ggplot2::ggplot(dat1 %>% subset(Time == 90), 
#'                       ggplot2::aes(x = DOSE, y = PCHG)) +
#'   ggplot2::geom_boxplot(aes(group = DOSE)) +
#'   xgx_theme() +
#'   xgx_scale_y_percentchangelog10() +
#'   ylab("Percent Change from Baseline") +
#'   xlab("Dose (mg)")
#' 
#' gg + 
#'   ggplot2::geom_smooth(method = "nlsLM", formula = y ~ E0 + Emax*x/(exp(logED50) + x), 
#'               method.args = list(start = list(Emax = -0.50, logED50 = log(25), E0 = 0)), 
#'               se = TRUE)
#'               
#' gg + 
#'   xgx_geom_smooth_emax()
#' 
#' @importFrom minpack.lm nlsLM
#' @importFrom stats nls
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_smooth
#' @export
xgx_geom_smooth_emax <- function(mapping = NULL, data = NULL, geom = "smooth",
                                 position = "identity", ..., method = "nlsLM", formula, 
                                 se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                 level = 0.95, method.args = list(), na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE){
  if(missing(formula)) {
    warning("Formula not specified.\nUsing default formula y ~ E0 + Emax*x/(ED50 + x), 
            initializing E0, Emax, and ED50 to 1, 
            and setting lower bound on ED50 to 0")
    formula = y ~ E0 + Emax*x/(ED50 + x)
    method.args$start = list(E0 = 1, Emax = 1, ED50 = 1)
    method.args$lower = c(-Inf, -Inf, 0)
  }

  ggplot2::stat_smooth(mapping = mapping, data = data, geom = geom, 
                       position = position, ..., method = method, formula = formula,
                       se = se, n = n, span = span, fullrange = fullrange, 
                       level = level, method.args = method.args, na.rm = na.rm,
                       show.legend = show.legend, inherit.aes = inherit.aes)
}
