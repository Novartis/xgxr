#' Saving plot, automatically annotating the status and denoting the filenames
#'
#' @param width width of plot
#' @param height height of plot
#' @param dirs list of directories.  If NULL or if directories missing, there 
#' is default behavior below
#'
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the Results 
#' folder, default getwd()
#' \item rscript_dir = Subdirectory of parent_dir that contains the Rscript 
#' used to generate the figure, default "./"
#' \item rscript_name= Name of the Rscript used to generate the figure, 
#' default "Name_Of_Script_Here.R"
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored, 
#' default "./"
#' \item filename_prefix = prefix of filename to be appended to filename_main
#' }
#' 
#' @param filename_main main part of the filename, excluding prefix and suffix.
#' no default
#' @param status status to be annotated
#' @param g ggplot plot object, default is ggplot::last_plot()
#' @param filetype file extension (e.g. "pdf","csv" etc.)
#' @param status_x x location of the status in plot
#' @param status_y y location of the status in plot
#' @param status_fontsize font size for status in plot
#'
#' @return ggplot2 plot object
#'
#' @examples
#' dirs <- list(parent_dir= "./",
#'              rscript_dir  = "./",
#'              rscript_name = "Example.R",
#'              results_dir  = "./",
#'              filename_prefix   = "Task01_")
#' data <- data.frame(x = 1:1000, y = stats::rnorm(1000))
#' ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point()
#' xgx_save(4, 4, dirs, "Example", "DRAFT")
#' 
#' @importFrom ggplot2 last_plot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom stats rnorm
#' @export
xgx_save <- function(width,
                     height,
                     dirs = NULL,
                     filename_main = NULL,
                     status = "DRAFT",
                     g = ggplot2::last_plot(),
                     filetype = "png",
                     status_x = Inf,
                     status_y = Inf,
                     status_fontsize = 7) {
  if (is.null(dirs$parent_dir)) {
    dirs$parent_dir <- getwd()
  }
  if (is.null(dirs$rscript_dir)) {
    dirs$rscript_dir <- "./"
  }
  if (is.null(dirs$rscript_name)) {
    dirs$rscript_name <- "default_script_name.R"
  }
  if (is.null(dirs$results_dir)) {
    dirs$results_dir <- "./"
  }
  if (is.null(dirs$filename_prefix)) {
    dirs$filename_prefix <- ""
  }
  if (is.null(filename_main)) {
    filename_main <- "Unnamed_Graph"
  }

  filedir <- file.path(dirs$results_dir)
  dirs$filename <- paste0(dirs$filename_prefix, filename_main, ".", filetype)

  g <- g + xgx_annotate_filenames(dirs)
  g <- g + xgx_annotate_status(status, x = status_x, y = status_y,
                               fontsize = status_fontsize)

  ggplot2::ggsave(plot = g, width = width, height = height,
                  file.path(filedir, dirs$filename))
  return(g)
}
