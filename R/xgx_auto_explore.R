#' Produce an xgx-styled report the given dataset using xgx R markdown templates,
#' or a user-provided R markdown template.  (Note: The R markdown template provided must
#' be formatted in a similar manner to that of the xgx R markdown templates to work.)
#' The working directory will contain a new directory (`xgx_autoexplore_output`) after running this function,
#'  which will contain a directory for the dataset, and futher a directory for the type of analysis / R markdown template.
#'
#' \code{xgx_auto_explore} returns an HTML and PDF document with plots
#' describing the provided dataset
#' 
#' This function can be used quickly to explore your data by generating 
#' overview plots before constructing non-linear mixed effects models.
#'
#' @param data_path Path (as a string) to the dataset that is to be analyzed
#' @param mapping A list of column name mappings from the
#' original (template) dataset column names
#' to the corresponding columns in the new dataset.
#' @param author_name The name of the author to be displayed on the template
#' @param multiple_dosing Whether or not to use a "Multiple" or "Single" Ascending dose template
#' @param dose_cmt Integer denoting the compartment for dosing records
#' @param pk_cmt An integer denoting the "compartment" containing the PK data. The "CMT" column will typically
#' have these integers, where each row may contain PK, PD, dosing or other events/observations data
#' @param pd_cmt An integer denoting the "compartment" containing the PD data, 
#' of the desired type  (continuous, ordinal, etc.). The "CMT" column will typically
#' have these integers, where each row may contain PK, PD, dosing or other events/observations data
#' @param pd_data_type The type of PD data - acceptable values exist in the following list: 
#' ["binary","continuous","count","ordinal","real_example","receptor_occupancy","time_to_event"]
#' @param steady_state_day used to denote the day of rich sampling of PK at steady state
#' @param time_between_doses dosing interval, has units to match the time variable of the dataset
#' @param rmd_template_name A custom output name for the generated Rmd file
#' @param rmd_template_path A user provided custom template (as a string)
#' @param rmd_output_path A custom output path for the generated Rmd file
#' (This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))
#' @param pdf_output_path  A custom output path for the generated PDF file
#' (This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))
#' @param html_output_path  A custom output path for the generated HTML file
#' (This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))
#' @param add_datetime Boolean indicating additon of a date stamp to the beginnning of the Rmd file
#' @param show_explanation Boolean indicating if the additional explanations (text in between figures) are needed for the user.
#'
#' @return NULL
#'
#' @examples
#' 
#' author_name = "Your Name Here"
#' show_explanation = FALSE
#' 
#' \dontrun{
#' # Try out the nonlinear_pkpd dataset with the
#' # Multiple Ascending Dose PK Rmd template
#' data_path <- "~/nonlinear_pkpd.csv"
#' 
#' # Specify the mapping of column names
#' mapping <- list(
#'   "TIME" = "TIM2",
#'   "NOMTIME" = "NT",
#'   "EVID" = 0,
#'   "CENS" = 0,
#'   "DOSE" = "MGKG",
#'   "TRTACT" = "TRT",
#'   "LIDV_NORM" = "LIDV/MGKG",
#'   "LIDV_UNIT" = "UNIT",
#'   "PROFDAY" = 1,
#'   "SEX" = 0,
#'   "WEIGHTB" = 0)
#' 
#' 
#' # 5 contains the PK Concentration in this dataset
#' pk_cmt = 5
#' # We don't need PD right now
#' pd_cmt = NULL
#' pd_data_type = NULL
#' 
#' 
#' dose_cmt = 1
#' steady_state_day = c(0, 6)
#' time_between_doses = 24
#' multiple_dosing = TRUE
#' 
#' output_directory = tempdir()
#' 
#' xgx_auto_explore(data_path = data_path,
#'                  mapping = mapping,
#'                  author_name = author_name,
#'                  pk_cmt = pk_cmt,
#'                  pd_cmt = pd_cmt,
#'                  dose_cmt = dose_cmt,
#'                  steady_state_day = steady_state_day,
#'                  time_between_doses = time_between_doses,
#'                  multiple_dosing = multiple_dosing,
#'                  pd_data_type = pd_data_type,
#'                  rmd_output_path = output_directory,
#'                  show_explanation = show_explanation)
#' }
#'     
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @export
xgx_auto_explore <- function(data_path = NULL,
                             mapping = list(),
                             author_name = NULL,
                             multiple_dosing = FALSE,
                             pk_cmt = NULL,
                             pd_cmt = NULL,
                             pd_data_type = NULL,
                             dose_cmt = NULL,
                             steady_state_day = NULL,
                             time_between_doses = NULL,
                             rmd_template_name = NULL,
                             rmd_template_path = NULL,
                             rmd_output_path = NULL,
                             pdf_output_path = NULL,
                             html_output_path = NULL,
                             add_datetime = TRUE,
                             show_explanation = TRUE) {

  working_dir <- getwd()

  # A specific file path to an R markdown file can be given; however,
  #   if the template path is not provided, the R markdown template
  #   from the xgx github will be downloaded.
  if (is.null(rmd_template_path)){
    rmd_str <- get_rmd_str(rmd_template_name = rmd_template_name,
                           multiple_dosing = multiple_dosing,
                           pk_cmt = pk_cmt,
                           pd_cmt = pd_cmt,
                           pd_data_type = pd_data_type)

    rmd_template_name <- tools::file_path_sans_ext(
                            get_rmd_name(rmd_template_name = rmd_template_name,
                                         multiple_dosing = multiple_dosing,
                                         pk_cmt = pk_cmt,
                                         pd_cmt = pd_cmt,
                                         pd_data_type = pd_data_type))
  } else{
    rmd_str <- readr::read_file(rmd_template_path)
    if (is.null(rmd_template_name)) {
      rmd_template_name <- tools::file_path_sans_ext(basename(rmd_template_path))
    }
  }

  # Setup default output paths
  dataset_name = tools::file_path_sans_ext(basename(data_path))
  autoexplore_out_dir = file.path(working_dir,
                                  "xgx_autoexplore_ouput",
                                  dataset_name,
                                  rmd_template_name)
  # Ensure that the directory exists by creating it - must be done iteratively for heirarchical directories
  dir.create(working_dir, showWarnings = FALSE)
  dir.create(file.path(working_dir, "xgx_autoexplore_ouput"), showWarnings = FALSE)
  dir.create(file.path(working_dir, "xgx_autoexplore_ouput", dataset_name), showWarnings = FALSE)
  dir.create(file.path(working_dir, "xgx_autoexplore_ouput", dataset_name, rmd_template_name), showWarnings = FALSE)

  
  if (is.null(rmd_output_path)) {
    rmd_output_path <- file.path(autoexplore_out_dir, paste0(rmd_template_name, ".Rmd"))
  }
  if (is.null(pdf_output_path)) {
    pdf_output_path <- file.path(autoexplore_out_dir, paste0(rmd_template_name, ".pdf"))
  }
  if (is.null(html_output_path)) {
    html_output_path <- file.path(autoexplore_out_dir, paste0(rmd_template_name, ".html"))
  }





  # Edit the Rmd template and your data to fit the standard dataset type
  rmd_ouput_path <- edit_rmd_template_str(rmd_str = rmd_str,
                                          mapping = mapping,
                                          rmd_output_path = rmd_output_path,
                                          data_path = data_path,
                                          pk_cmt = pk_cmt,
                                          pd_cmt = pd_cmt,
                                          dose_cmt = dose_cmt,
                                          steady_state_day = steady_state_day,
                                          time_between_doses = time_between_doses,
                                          author_name = author_name,
                                          add_datetime = add_datetime,
                                          show_explanation = show_explanation)


  # Render and save the HTML document
  rmarkdown::render(input = rmd_output_path,
                    output_file = html_output_path,
                    output_dir = autoexplore_out_dir,
                    output_format = "html_document",
                    quiet = TRUE)

  # Render and save the PDF
  rmarkdown::render(input = rmd_output_path,
                    output_file = pdf_output_path,
                    output_dir = autoexplore_out_dir,
                    output_format = "pdf_document",
                    quiet = TRUE)
}




#' Edit a Rmd Template from xgx
#'
#' \code{edit_rmd_template_str} returns a path to the altered Rmd template
#' 
#' @param rmd_str A character string containing the Rmd template raw characters
#' @param mapping A list of column name mappings from the
#' original (template) dataset column names
#' to the corresponding columns in the new dataset
#' @param rmd_output_path A custom output path for the generated Rmd file
#' (This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))
#' @param data_path Path (as a string) to the dataset that is to be analyzed
#' @param multiple_dosing if FALSE use single ascending dose template, if TRUE use multiple
#' @param pk_cmt An integer denoting the "compartment" containing the PK data. The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data, potentially of different types (continuous, ordinal, etc.)
#' @param pd_cmt An integer denoting the "compartment" containing the PD data, of the desired type  (continuous, ordinal, etc.). The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data
#' @param dose_cmt CMT associated with dosing event
#' @param steady_state_day For multiple ascending dose, what day is steady state rich profile?
#' @param time_between_doses time interval between doses
#' @param author_name The name of the author to be displayed on the template
#' @param add_datetime Boolean indicating additon of a date stamp to the beginnning of the Rmd file
#' @param show_explanation Boolean indicating if the additional explanations (text in between figures) are needed for the user.
#' 
#' @return A string of the new R markdown template
#'
#' @importFrom glue glue
#' @importFrom Hmisc escapeRegex
#' @importFrom readr read_file
#' @importFrom stringr str_replace
#' @importFrom utils capture.output
#' @importFrom utils getParseData
#' @importFrom utils read.csv
#' @importFrom utils tail
#' 
#' @export
edit_rmd_template_str <- function(rmd_str = NULL,
                                  mapping = NULL,
                                  rmd_output_path = NULL,
                                  data_path = NULL,
                                  multiple_dosing = FALSE,
                                  pk_cmt = NULL,
                                  pd_cmt = NULL,
                                  dose_cmt = NULL,
                                  steady_state_day = NULL,
                                  time_between_doses = NULL,
                                  author_name = NULL,
                                  add_datetime = TRUE,
                                  show_explanation = TRUE) {
  
  token <- parent <- NULL

  author_name_re <- 'author: \\"(.*)\\"'

  user_data <- utils::read.csv(data_path)


  # Alter the path to the data, to match the user given path
  if (!is.null(data_path)) {

    # Edit the Rmd string to contain the csv desired filepath
    user_data_path_replacement_re = paste0("\\1", '\\"',
                                           Hmisc::escapeRegex(data_path),
                                           '\\"\\)')
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = "(read.csv\\()(.*)",
                                    replacement = user_data_path_replacement_re)
  }

  # Change the column name mapping
  if (!is.null(mapping)) {

    # Alter dataset in Rmd
    #   - Note: `mutate` is assumed to be used at the top of an Rmd template
    #           similar to all xgx Rmd templates

    # Find first location of `mutate` function
    mutate_loc <- stringr::str_locate(rmd_str, "mutate\\(")[1, "start"]

    # Get the string starting at this location
    mutate_start_rmd_str <- substr(rmd_str,
                                   start = mutate_loc,
                                   stop = nchar(rmd_str))

    # Regular Expressions do not work here, becuase comments can contain parentheses
    # So we will get the `mutate` full expression by parsing
    mutate_expr <- parse(text = mutate_start_rmd_str, n = 1)
    mutate_expr_parse_data <- utils::getParseData(mutate_expr, includeText = TRUE)
    mutate_expr_str <- mutate_expr_parse_data[1, "text"]

    # Split the main `mutate` function parameters into 
    #   'right' and 'left' hand side expressions
    main_parent <- mutate_expr_parse_data[1,"id"]
    orig_mapping_right <- mutate_expr_parse_data %>%
                              subset(token == "expr") %>%
                              subset(parent == main_parent) %>%
                              select(text)
    orig_mapping_right <- utils::tail(orig_mapping_right[[1]], -1)

    orig_mapping_left <- mutate_expr_parse_data %>% 
                              subset(token == "SYMBOL_SUB") %>%
                              subset(parent == main_parent) %>%
                              select(text)
    
    # Original mapping is stored as a list with the 
    #   names as the 'right hand' expressions, and values as 'left hand' exprs
    orig_mapping <- orig_mapping_right
    names(orig_mapping) <- orig_mapping_left[[1]]

    # Now we add the original map to the new map
    #   if the new mapping is missing anything in the original map
    for (old_col in names(orig_mapping)) {
      # Value for key value pair in orig_mapping
      old_col_value <- orig_mapping[[old_col]]
      
      # Add columns from old mapping that are not yet present in mapping
      if (!(old_col %in% names(mapping))) {
        mapping[old_col] <- old_col_value
      }
    }

    # The new mapping must be stored as a string in order to insert
    new_mutate_str <- paste(utils::capture.output(dput(mapply(as.name, mapping))),
                            sep='\n',
                            collapse = "")
    # Change 'list' to 'mutate' in mapping string representation
    new_mutate_str <- stringr::str_replace(string = new_mutate_str,
                                           pattern = "list",
                                           replacement = "mutate")
    # Remove newlines, to add them in in a more consistent manner later
    new_mutate_str <- stringr::str_replace_all(string = new_mutate_str,
                                               pattern = "\\\\n",
                                               replacement = "")
    # Remove string punctuation such that expressions are evaluated correctly
    new_mutate_str <- stringr::str_replace_all(string = new_mutate_str,
                                           pattern = "`",
                                           replacement = "")

    # New Rmd
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = Hmisc::escapeRegex(mutate_expr_str),
                                    replacement = new_mutate_str)
  }

  # Change the PK compartment
  if (!is.null(pk_cmt)) {
    pattern <- "PK_CMT\\s*=\\s*(\\d)"
    replace_str <- glue::glue("PK_CMT = {pk_cmt}")
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = pattern,
                                    replacement = replace_str)
  }

  # Change the PD compartment
  if (!is.null(pd_cmt)) {
    pattern <- "PD_CMT\\s*=\\s*(\\d)"
    replace_str <- glue::glue("PD_CMT = {pd_cmt}")
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = pattern,
                                    replacement = replace_str)
  }

  # Change the dose compartment
  if (!is.null(dose_cmt)) {
    pattern <- "DOSE_CMT\\s*=\\s*(\\d)"
    replace_str <- glue::glue("DOSE_CMT = {dose_cmt}")
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = pattern,
                                    replacement = replace_str)
  }

  # Change the steady state day
  if (!is.null(steady_state_day)) {
    pattern <- "SS_PROFDAY\\s*=\\s*(\\d)"
    replace_str <- glue::glue("SS_PROFDAY = {steady_state_day}")
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = pattern,
                                    replacement = replace_str)
  }

  # Change tau
  if (!is.null(time_between_doses)) {
    pattern <- "TAU\\s*=\\s*(\\d)"
    replace_str <- glue::glue("TAU = {time_between_doses}")
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = pattern,
                                    replacement = replace_str)
  }

  # Add author name
  if (!is.null(author_name)) {
    replacement_str <- glue::glue('author: \"{author_name}\"')
    rmd_str <- stringr::str_replace(string = rmd_str,
                                pattern = author_name_re,
                                replacement = replacement_str)
  }

  # Add datetime
  if (add_datetime) {
    date_re <- 'date: \\"(.*)\\"'

    rmd_date_str <- stringr::str_match(string = rmd_str,
                                       pattern = date_re)
    
    datetime_str <- "date: \"`r format(Sys.time(), '%d %B, %Y')`\""

    # if date already present, just replace it
    if (is.null(rmd_date_str[1])) {
      search_date_re <- date_re
      replacement_str <- datetime_str
    }
    # Otherwise, add it after the author name
    else {
      search_date_re <- author_name_re
      author_str <- stringr::str_match(string = rmd_str,
                                       pattern = author_name_re)[1]
      replacement_str <- paste0(author_str, '\n', datetime_str)
    }

    rmd_str <- stringr::str_replace(string = rmd_str,
                                pattern = search_date_re,
                                replacement = Hmisc::escapeRegex(replacement_str))
  }

  if(!(show_explanation)) {
    start_comment <- "<!--START_EXPLANATION-->"
    end_comment <- "<!--END_EXPLANATION-->"
    pattern <- paste0(start_comment, "(\\s\\S)*", end_comment)
    replace <- "" # paste0("<!--", "\\1", "-->")
    rmd_str <- stringr::str_replace_all(rmd_str,
                                        pattern = "(START_EXPLANATION-->|<!--END_EXPLANATION)",
                                        replacement = replace)
  }
  
  # Add source files
#   rmd_str <- stringr::str_replace_all(rmd_str,
#                                       pattern = "library\\(xgxr\\)",
#                                       replacement = Hmisc::escapeRegex("library(xgxr)
# # For testing:
# source('~/xgxr/R/xgx_conf_int.R', echo=FALSE)
# source('~/xgxr/R/xgx_stat_ci.R', echo=FALSE)
# source('~/xgxr/Rdev/xgx_ordinal_regression_plot.R', echo=FALSE)
# source('~/xgxr/Rdev/xgx_stat_smooth.R', echo=FALSE)"))

  # Save the R markdown document
  dir.create(dirname(rmd_output_path), showWarnings = FALSE)
  fileConn <- file(rmd_output_path, 'w')
  writeChar(rmd_str, fileConn)
  close(fileConn)

  return(rmd_output_path)
}







#' Determine the name of a Rmd template
#'
#' \code{get_rmd_name} returns a name for an Rmd template, based on the desired PKPD parameters
#' 
#'
#' @param rmd_template_name A custom output name for the generated Rmd file
#' @param multiple_dosing if FALSE use single ascending dose template, if TRUE use multiple
#' @param pk_cmt An integer denoting the "compartment" containing the PK data. The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data, potentially of different types (continuous, ordinal, etc.)
#' @param pd_cmt An integer denoting the "compartment" containing the PD data, of the desired type  (continuous, ordinal, etc.). The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data
#' @param pd_data_type The type of PD data - acceptable values exist in the following list: ["binary","continuous","count","ordinal","real_example","receptor_occupancy","time_to_event"]
#' 
#' @return a string for the Rmd template name
#'
#'     
#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @export
get_rmd_name <- function(rmd_template_name = NULL,
                         multiple_dosing = FALSE,
                         pk_cmt = NULL,
                         pd_cmt = NULL,
                         pd_data_type = NULL) {

  if (!is.null(rmd_template_name)) {
    # For Adverse_Events, Oncology_Efficacy_Plots
    # Perhaps Multiple_Ascending_Dose_PK_KeyPlots ?
    return(paste0(rmd_template_name, ".Rmd"))
  }

  allowable_pd_data_types <- c("binary",
                               "continuous",
                               "count",
                               "ordinal",
                               "real_example",
                               "receptor_occupancy",
                               "time_to_event")

  if (!(is.null(pd_data_type))) {
    if (!(pd_data_type %in% allowable_pd_data_types)) {
      warning(glue::glue("The provided pd_data_type `{pd_data_type}` is not allowable.
                         Please choose a value from the list {allowable_pd_data_types}"))
    }
  }
  
  # Construct the filename via the standard xgx rmd template filename format
  pk_str <- if (!is.null(pk_cmt)) "PK" else ""
  pd_str <- if (!is.null(pd_cmt)) "PD" else ""
  if (multiple_dosing) {
    multiple_dosing_str <- "Multiple_Ascending"
  }
  else{
    multiple_dosing_str <- "Single_Ascending"
  }

  if (!is.null(pd_data_type) & !is.null(pd_cmt)){
    rmd_fname <- glue::glue("{multiple_dosing_str}_Dose_{pk_str}{pd_str}_{pd_data_type}.Rmd")
  }
  else {
    rmd_fname <- glue::glue("{multiple_dosing_str}_Dose_PK.Rmd")
  }

  return(rmd_fname)
}


# Extract the Rmd document from the xgx github
#   if the doesn't work, pull the file from the cached xgxr github
#   backup directoryinstead
#'
#' \code{get_rmd_str} returns a Rmd template string, based on the desired PKPD parameters
#' 
#'
#' @param rmd_template_name A custom output name for the generated Rmd file
#' @param multiple_dosing if FALSE use single ascending dose template, if TRUE use multiple
#' @param pk_cmt An integer denoting the "compartment" containing the PK data. The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data, potentially of different types (continuous, ordinal, etc.)
#' @param pd_cmt An integer denoting the "compartment" containing the PD data, of the desired type  (continuous, ordinal, etc.). The "CMT" column will typically
#' have these integers, where each row may contain either PK or PD data
#' @param pd_data_type The type of PD data - acceptable values exist in the following list: ["binary","continuous","count","ordinal","real_example","receptor_occupancy","time_to_event"]
#' 
#' @return a string for the Rmd template name
#'
#'
#' @importFrom RCurl getURL
#' @importFrom readr read_file
#' @importFrom stringr str_replace
#' 
#' @export
get_rmd_str <- function(rmd_template_name = NULL,
                        multiple_dosing = FALSE,
                        pk_cmt = NULL,
                        pd_cmt = NULL,
                        pd_data_type = NULL){

  rmd_fname <- get_rmd_name(rmd_template_name = rmd_template_name,
                            multiple_dosing = multiple_dosing,
                            pk_cmt = pk_cmt,
                            pd_cmt = pd_cmt,
                            pd_data_type = pd_data_type)

  # Try github first
  git_url <- "https://raw.githubusercontent.com/Novartis/xgx/master/Rmarkdown/"
  full_url <- paste0(git_url, rmd_fname)

  # Read the Rmd from github into a string
  rmd_str <- RCurl::getURL(full_url, ssl.verifypeer = FALSE)

  # If no internet connection / unsuccessful, try the local files in xgxr
  if (is.null(rmd_str)) {
    rmd_str <- readr::read_file("../data/xgx_Rmd/" + rmd_fname)
  }
  else if (rmd_str == "404: Not Found") {
    rmd_str <- readr::read_file("../data/xgx_Rmd/" + rmd_fname)
  }

  return(rmd_str)
}

