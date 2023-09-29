# Here, we're working on this issue, but I got confused.  And I"m saving this for now as a placeholder
# https://github.com/Novartis/xgxr/issues/41

# this is the code to run, but it gives an error, below
#
# dummy_data <- data.frame(x = as.Date(c("2015-01-01", "2016-01-01")), y = c(1,2))
# ggplot(dummy_data, aes(x = x, y = y)) + geom_point() + xgx_annotate_status_new("DRAFT")
#
# Error: object of type 'closure' is not subsettable

xgx_annotate_status_new <- function(status = "DRAFT",
                                x = Inf, y = Inf, color = "grey",
                                hjust = 1.2, vjust = 1.2,
                                fontsize = 7, fontface = "bold",
                                alpha = 0.5, ...) {

#THIS IS JUST THE ggplot::annotate() function with one line added
annotate_new <- function (geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, 
                                 ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ..., 
                                 na.rm = FALSE) 
{
  
  #TO DO - THINK ABOUT IF WE WANT TO DO THIS CHANGE ONLY FOR SPECIFIC DATA TYPES OR ALL DATA TYPES
  #THIS IS THE ONLY NEW PART.  BUT IT GIVES AN ERROR AND I DON'T KNOW WHY
  x_new <- max(na.omit(data$x))
  
  if (is_string(geom, c("abline", "hline", "vline"))) {
    cli::cli_warn(c("{.arg geom} must not be {.val {geom}}.", 
                    i = "Please use {.fn {paste0('geom_', geom)}} directly instead."))
  }
  position <- compact(list(x = x_new, xmin = xmin, xmax = xmax, 
                           xend = xend, y = y, ymin = ymin, ymax = ymax, yend = yend))
  aesthetics <- c(position, list(...))
  lengths <- lengths(aesthetics)
  n <- unique0(lengths)
  if (length(n) > 1L) {
    n <- setdiff(n, 1L)
  }
  if (length(n) > 1L) {
    bad <- lengths != 1L
    details <- paste0(names(aesthetics)[bad], " (", lengths[bad], 
                      ")")
    cli::cli_abort("Unequal parameter lengths: {details}")
  }
  data <- data_frame0(!!!position, .size = n)

  
  layer(geom = geom, params = list(na.rm = na.rm, ...), stat = StatIdentity, 
        position = PositionIdentity, data = data, mapping = aes_all(names(data)), 
        inherit.aes = FALSE, show.legend = FALSE)
}
 
annotate_new("text", x = x, y = y,
             label = status, color = color,
             hjust = hjust, vjust = vjust,
             cex = fontsize, fontface = fontface,
             alpha = alpha, ...) 
  
}