#' Saving table as an image, also labeling the program that created the table 
#' and where the table is stored
#'
#' @param data  data.frame or table of results
#' @param dirs list of directories.  If NULL or if directories missing, there 
#' is default behavior below
#'
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the 
#' Results folder, default getwd()
#' \item rscript_dir = Subdirectory of parent_dir that contains the Rscript 
#' used to generate the figure, default "./"
#' \item rscript_name= Name of the Rscript used to generate the figure, 
#' default "Name_Of_Script_Here.R"
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored, 
#' default "./"
#' \item filename_prefix = prefix of filename to be appended to filename_main
#' }
#' @param filename_main main part of the filename, excluding prefix and 
#' extension.  no default
#'
#' @return ggplot2 plot object
#'
#' @examples
#' \dontrun{
#' dirs <- list(parent_dir  = "./",
#'              rscript_dir = "./",
#'              rscript_name = "example.R",
#'              results_dir = "./",
#'              filename_prefix = "example_")
#' data <- data.frame(x = c(1, 2), y = c(1, 2))
#' xgx_save_table(data, dirs = dirs, filename_main = "test")
#' }
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate_all
#' @importFrom utils write.csv
#' @importFrom magrittr "%>%"
#' @export
xgx_save_table <- function(data, dirs = NULL, filename_main = NULL) {
  if (is.null(dirs$parent_dir)) {
    stop("The parent directory for your programs and results must be specified: dirs$parent_dir")
  }
  if (is.null(dirs$rscript_dir)) {
    stop("The project directory where your R scripts are stored must be specified: dirs$rscript_dir")
  }
  if (is.null(dirs$rscript_name)) {
    stop("The name of the R script that saves this plot must be specified: dirs$rscript_name")
  }
  if (is.null(dirs$results_dir)) {
    stop("The results directory where your outputs are stored must be specified: dirs$results_dir")
  }
  if (is.null(dirs$filename_prefix)) {
    dirs$filename_prefix <- ""
  }
  if (is.null(dirs$filename_main)) {
    filename_main <- "unnamed_table_"
  }

  filedir <- file.path(dirs$results_dir)
  dirs$filename <- paste0(dirs$filename_prefix, filename_main, ".csv")

  caption <- c("", dirs$parent_dir,
               file.path(dirs$rscript_dir, dirs$rscript_name),
               file.path(dirs$results_dir, dirs$filename),
               file.path("Created: ", Sys.time()))

  caption_row <- data[1, ] %>%
    dplyr::mutate_all(function(x) {x <- ""})
  caption_row <- dplyr::bind_rows(caption_row, caption_row, caption_row,
                                  caption_row, caption_row)
  caption_row[, 1] <- caption

  data_append <- data %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::bind_rows(caption_row)

  utils::write.csv(data_append, file.path(filedir, dirs$filename),
                   quote = FALSE, row.names = FALSE)
  return(data_append)
}
