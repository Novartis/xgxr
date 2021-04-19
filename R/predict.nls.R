#' predict.nls
#' 
#' @param object Object of class inheriting from "nls"
#' @param newdata An optional data frame in which to look for variables with which to predict. 
#' If omitted, the fitted values are used.
#' @param se.fit A switch indicating if standard errors are required.
#' @param interval Type of interval calculation, "none" or "confidence"
#' @param level Level of confidence interval to use
#' @param ... additional arguments affecting the predictions produced.
#'
#' @return \code{predict.nls} produces a vector of predictions or a matrix of predictions and 
#' bounds with column names \code{fit}, \code{lwr}, and \code{upr} if interval is set. 
#' 
#' If \code{se.fit} is \code{TRUE}, a list with the following components is returned:
#' 
#' \item{fit}{vector or matrix as above}
#' 
#' \item{se.fit}{standard error of predicted means}
#' 
#' \item{residual.scale}{residual standard deviations}
#' 
#' \item{df}{degrees of freedom for residual}
#'
#' @examples 
#' 
#' set.seed(12345)
#' data_to_plot <- data.frame(x1 = rep(c(0, 25, 50, 100, 200, 400, 600), 10)) %>%
#'   dplyr::mutate(AUC = x1*rlnorm(length(x1), 0, 0.3),
#'          x2 = x1*stats::rlnorm(length(x1), 0, 0.3),
#'          Response = (15 + 50*x2/(20+x2))*stats::rlnorm(length(x2), 0, 0.3))
#' 
#' 
#' gg <- ggplot2::ggplot(data = data_to_plot, ggplot2::aes(x = AUC, y = Response)) + 
#'   ggplot2::geom_point() + 
#'   xgx_geom_smooth(method = "nls",  
#'                   method.args = list(formula = y ~ E0 + Emax* x / (EC50 + x),
#'                                      start = list(E0 = 15, Emax = 50, EC50 = 20) ), 
#'                   color = "black", size = 0.5, alpha = 0.25)
#' gg
#' 
#' mod <- stats::nls(formula = Response ~ E0 + Emax * AUC / (EC50 + AUC), 
#' data = data_to_plot, 
#' start = list(E0 = 15, Emax = 50, EC50 = 20))
#' 
#' predict.nls(mod)
#' 
#' predict.nls(mod, se.fit = TRUE)
#' 
#' predict.nls(mod, 
#'             newdata = data.frame(AUC = c(0, 25, 50, 100, 200, 400, 600)), 
#'             se.fit = TRUE)
#'             
#' predict.nls(mod, 
#'             newdata = data.frame(AUC = c(0, 25, 50, 100, 200, 400, 600)), 
#'             se.fit = TRUE, interval = "confidence", level = 0.95)
#'             
#' predict(mod, 
#'             newdata = data.frame(AUC = c(0, 25, 50, 100, 200, 400, 600)), 
#'             se.fit = TRUE, interval = "confidence", level = 0.95)
#'
#' @importFrom Deriv Deriv
#' @importFrom stats nls
#' @exportS3Method stats::predict
#' @export predict.nls
predict.nls <- function(object, newdata = NULL, se.fit = FALSE, interval = "none", level = 0.95, ...){
  
  pred <- list()
  
  # function to calculate gradient wrt model parameters
  # value is the function value
  # grad is the gradient
  fun_grad <- function(form, newdata, pars, se.fit){
    
    # extract the model parameters to the local environment
    list2env(pars %>% as.list(), envir = environment())
    
    ret <- list()
    
    if(se.fit){
      ret$grad <- list()
    }
    
    
    for(i in 1:length(newdata[,1])){
      
      if(length(newdata[1,]) > 1){  
        for(j in names(newdata[i,])){
          assign(j, newdata[i,j])
        }
      }else{
        j = names(newdata[1])
        assign(j, newdata[i,j])
      }
      
      ret$value[i] <- eval(form[[3L]]) # this is the value of the formula
      
      if(se.fit){
        ret$grad[[i]] <- eval(Deriv::Deriv(form, names(pars), cache.exp = FALSE)) %>% as.list()
        
        if(is.null(names(ret$grad[[i]]))){
          names(ret$grad[[i]]) <- names(pars)
        }
      }
    }
    
    
    if(se.fit){
      ret$grad <- dplyr::bind_rows(ret$grad) %>% as.matrix
    }
    
    return(ret)
  }
  
  if(is.null(newdata)){
    fg <- list()
    fg$value <- as.numeric(object$m$fitted())
    fg$grad <- object$m$gradient()
    
  }else{
    fg <- fun_grad(form = object$m$formula(), newdata, pars = object$m$getPars(), se.fit)
    
  }
  
  
  f.new <- fg$value # value of function
  
  if(se.fit){
    pred$fit <- f.new
    
    grad.new <- fg$grad # value of gradient
    
    vcov <- vcov(object)
    GS = rowSums((grad.new%*%vcov)*grad.new)
    
    if(interval == "confidence"){
      
      alpha = 1 - level
      deltaf <- sqrt(GS)*qt(1 - alpha/2, df = summary(object)$df[2])
      
      pred$fit <- data.frame(fit = pred$fit)
      pred$fit$lwr <- f.new - deltaf
      pred$fit$upr <- f.new + deltaf
    }
    
    pred$se.fit <- sqrt(GS)
    pred$df <- summary(object)$df[2]
    
  }else{
    pred <- f.new
  }
  
  return(pred)
}

