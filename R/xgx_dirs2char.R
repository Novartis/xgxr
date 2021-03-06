#' Append filenames to bottom of the plot 
#' 
#' \code{xgx_dirs2char} returns a character variable based on the dirs list.
#' The caption gives the filename
#' 
#' @param dirs list containing directories and filenames.  It must contain 
#' five fields
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the Results 
#' folder
#' \item rscript_dir = Subdirectory ofparent_dir that contains the Rscript 
#' used to generate the figure
#' \item rscript_name= Name of the Rscript used to generate the figure
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored
#' \item filename    = Filename
#' }
#' @param include_time is logical with default TRUE.  If TRUE, it includes 
#' date / time in the output character
#' 
#' @return character
#'
#' @examples
#' dirs <- list(parent_dir = "/your/parent/path/",
#'             rscript_dir = "./Rscripts/",
#'             rscript_name = "Example.R",
#'             results_dir = "./Results/",
#'             filename = "your_file_name.png")
#' caption <- xgx_dirs2char(dirs)
#' 
#' @export
xgx_dirs2char <- function(dirs, include_time = TRUE) {
  # check to make sure all filenames dirs
  if (typeof(dirs)!="list") {
    stop("dirs variable must be a list")
  }
  missing_filenames <- setdiff(c("parent_dir", "rscript_dir", "rscript_name",
                                 "results_dir", "filename"),
                              names(dirs))
  if (length(missing_filenames) > 0) {
    stop(paste("Fields missing from dirs = ", missing_filenames))
  }

  output <- paste0(dirs$parent_dir, "\n",
                   file.path(dirs$rscript_dir, dirs$rscript_name), "\n",
                   file.path(dirs$results_dir, dirs$filename))

  if (include_time) {
    output <- paste0(output, "\n", "Created: ", Sys.time())
  }

  return(output)
}
