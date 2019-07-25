#' Create a new xgx plot
#'
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify.
#' @param mapping As in ggplot2; Default list of aesthetic mappings to use for plot. Must define x, y, and group for xgx_spaghetti.
#' @param ... Other arguments passed on to methods. Not currently used. 
#' @param environment If an variable defined in the aesthetic mapping is not found in the data, ggplot will look for it in this environment. It defaults to using the environment in which @ggplot() is called.
#'
#' xgx_plot
#'
#' 
#' @examples  
#' time = rep(seq(1,10),5)
#' id = sort(rep(seq(1,5), 10))
#' conc = exp(-time)*sort(rep(stats::rlnorm(5),10))
#' 
#' data = data.frame(time = time, concentration  = conc, id = id)
#' xgx_plot(data = data, mapping = ggplot2::aes(x = time, y = concentration, group = id)) + 
#'   ggplot2::geom_line() + 
#'   ggplot2::geom_point()
#'   
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom stats rlnorm
#' @export
xgx_plot <-  function(data = NULL, mapping = ggplot2::aes(), ..., environment = parent.frame()){

  gg <- ggplot2::ggplot(data = data, mapping = mapping, ..., environment = environment) +
    xgx_theme()
  
  ret <- gg
  
  return(ret);
}