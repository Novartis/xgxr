#' Plot Emax fit to data
#' Uses nls, predictdf.nls, and stat_smooth to display Emax model fit to data
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
#'   geom_smooth(method = "nls", formula = y ~ Emax*x/(ED50 + x), 
#'               method.args = list(start = list(Emax = -0.50, ED50 = 25)), 
#'               se = TRUE)
#'               
#' gg + 
#'   xgx_geom_smooth_emax()
#'
#' @importFrom stats nls
#' @importFrom ggplot2 geom_smooth
#' @export
xgx_geom_smooth_emax <- function(mapping = NULL, data = NULL, geom = "smooth",
                                 position = "identity", ..., method = "nls", formula, 
                                 se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                 level = 0.95, method.args = list(), na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE){
  if(missing(formula)) {
    warning("Formula not specified.\nUsing default formula y ~ Emax*x/(ED50 + x)")
    formula = y ~ Emax*x/(ED50 + x)
  }
  
  variables <- attr(terms(formula),"variables") %>% 
    as.character %>% 
    setdiff(c("list","y","x"))
  
  if(is.null(method.args$start)){
    warning("No starting values specified.\nInitializing ", paste0(variables,collapse = ", ")," to 1")
    
    for(ivar in variables){
      method.args$start[[ivar]] <- 1
    }
    
  }else{
    for(ivar in variables){
      if(is.null(method.args$start[[ivar]])) warning(paste0("No starting value specified for ", ivar, "\ninitializing to 1"))
      method.args$start[[ivar]] <- 1
    }
  }

  ggplot2::stat_smooth(mapping = mapping, data = data, geom = geom, 
                       position = position, ..., method = method, formula = formula,
                       se = se, n = n, span = span, fullrange = fullrange, 
                       level = level, method.args = method.args, na.rm = na.rm,
                       show.legend = show.legend, inherit.aes = inherit.aes)
}


#' Prediction data frame for nls
#' Get predictions with standard errors into data frame for use with geom_smooth
#' 
#' \code{predictdf.nls} calculates CI for a model fit of class nls based on the "delta-method" 
#' http://sia.webpopix.org/nonlinearRegression.html#confidence-intervals-and-prediction-intervals)
#'
#' CI = [ f(x0, beta) + qt_(alpha/2, n - d) * se(f(x0, beta)),
#'       f(x0, beta) + qt_(1 - alpha/2, n - d) * se(f(x0, beta))]
#'
#' where:
#' beta = vector of parameter estimates
#' x = independent variable
#' se(f(x0, beta)) = sqrt( delta(f)(x0, beta) * Var(beta) * (delta(f)(x0, beta))' )
#' delta(f) is the gradient of f
#'
#' @param model nls object
#' @param xseq newdata
#' @param se Display confidence interval around smooth?
#' @param level Level of confidence interval to use
#'
#' @return dataframe with x and y values, if se is TRUE dataframe also includes ymin and ymax
#'
#' @importFrom stats deriv
#' @importFrom ggplot2 predictdf.default
#' @export
predictdf.nls <- function(model, xseq, se, level) {
  
  if(se){
    # function to calculate gradient wrt model parameters
    fun_grad <- function(model, x){
      # extract the model parameters to the local environment
      list2env(model$m$getPars() %>% as.list(), envir = environment())
      
      # deriv returns the function value along with the gradient
      eval(stats::deriv(model$m$formula(), names(model$m$getPars())))
    }
    
    # calculate the model prediction, as well as the gradient, at the supplied x values 
    f.new <- fun_grad(model, xseq)
    
    # extract the gradient
    grad.new <- attr(f.new, "gradient")
    vcov <- vcov(model)
    GS = rowSums((grad.new%*%vcov)*grad.new)
    
    alpha = 1 - level
    deltaf <- sqrt(GS)*qt(1 - alpha/2, df = summary(model)$df[2])
    
    pred <- data.frame(x = xseq, y = f.new, ymin = f.new - deltaf, ymax = f.new + deltaf)

  }else{

    pred <- ggplot2:::predictdf.default(model, xseq, se, level)
  }
  
  return(pred)
}



