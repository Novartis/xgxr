#' Plot all functions for the given dataset
#'
#' \code{xgx_auto_explore} returns an HTML and PDF document with plots
#' describing the provided dataset
#' 
#' This function can be used quickly explore your data by generating 
#' overview plots before constructing non-linear mixed effects models.
#'
#' @param data_path Path (as a string) to the dataset that is to be analyzed
#' @param mapping A list of column name mappings from the
#' original (template) dataset column names
#' to the corresponding columns in the new dataset
#' @param author_name The name of the author to be displayed on the template
#' @param multiple_dosing 
#' @param pk_cmt 
#' @param pd_cmt
#' @param pd_data_type 
#' @param rmd_template_path 
#' @param rmd_output_path 
#' @param pdf_output_path 
#' @param html_output_path 
#' @param alter_datetime 
#' @param show_explanation 
#'
#' @return NULL
#'
#' @examples
#'     Name mapping: 
#'     
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @importFrom magrittr "%>%"
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
                             rmd_template_path = NULL,
                             rmd_output_path = NULL,
                             pdf_output_path = NULL,
                             html_output_path = NULL,
                             alter_datetime = TRUE,
                             show_explanation = TRUE) {
  
  # Setup default output paths
  file_prefix = "xgx_autoexplore_"
  analysis_fname <- tools::file_path_sans_ext(
                           get_rmd_name(multiple_dosing = multiple_dosing,
                                        pk_cmt = pk_cmt,
                                        pd_cmt = pd_cmt,
                                        pd_data_type = pd_data_type))
  if (is.null(rmd_output_path)) {
    working_dir <- getwd()
    rmd_output_path <- file.path(working_dir, paste0(file_prefix, analysis_fname, ".Rmd"))
  }
  if (is.null(pdf_output_path)) {
    working_dir <- getwd()
    pdf_output_path <- file.path(working_dir, paste0(file_prefix, analysis_fname, ".pdf"))
  }
  if (is.null(html_output_path)) {
    working_dir <- getwd()
    html_output_path <- file.path(working_dir, paste0(file_prefix, analysis_fname, ".html"))
  }


  # A specific file path to an R markdown file can be given; however,
  #   if the template path is not provided, the R markdown template
  #   from the xgx github will be downloaded.
  if (is.null(rmd_template_path)){
    rmd_str <- get_rmd_str(multiple_dosing = multiple_dosing,
                           pk_cmt = pk_cmt,
                           pd_cmt = pd_cmt,
                           pd_data_type = pd_data_type)
  } else{
    rmd_str <- readr::read_file(rmd_template_path)
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
                                          alter_datetime = alter_datetime,
                                          show_explanation = show_explanation)


  # Render and save the HTML document
  rmarkdown::render(input = rmd_output_path,
                    output_file = html_output_path,
                    output_format = "html_document",
                    quiet = TRUE)

  # Render and save the PDF
  rmarkdown::render(input = rmd_output_path,
                    output_file = pdf_output_path,
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
#' @param rmd_output_path 
#' @param data_path Path (as a string) to the dataset that is to be analyzed
#' @param pk_cmt 
#' @param pd_cmt
#' @param author_name The name of the author to be displayed on the template
#' @param pd_data_type 
#' @param alter_datetime 
#' @param show_explanation 
#'
#' @return NULL
#'
#' @examples
#'     Name mapping: 
#'     
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @importFrom(magrittr, "%>%")
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
                                  alter_datetime = TRUE,
                                  show_explanation = TRUE) {
  library(magrittr)
  library(dplyr)

  author_name_re <- 'author: \\"(.*)\\"'

  user_data <- read.csv(data_path)


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
    mutate_expr_parse_data <- getParseData(mutate_expr, includeText = TRUE)
    mutate_expr_str <- mutate_expr_parse_data[1, "text"]

    # Split the main `mutate` function parameters into 
    #   'right' and 'left' hand side expressions
    main_parent <- mutate_expr_parse_data[1,"id"]
    orig_mapping_right <- mutate_expr_parse_data %>%
                              subset(token == "expr") %>%
                              subset(parent == main_parent) %>%
                              select(text)
    orig_mapping_right <- tail(orig_mapping_right[[1]], -1)

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
    new_mutate_str <- paste(capture.output(dput(mapply(as.name, mapping))),
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
  if (alter_datetime) {
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
    # pattern = "(---\n\n*|```\n\n*)(#+[\\s\\S]*?)(?=\\s*```\\{r)"
    # texts = stringr::str_match_all(rmd_str,
    #                                pattern = pattern)[[1]][,3]
    # print(texts)
    # for (text in as.list(texts)){
    #   explanation_texts <- stringr::str_match_all(string = text,
    #                                     pattern = "\n\\s*[^#][\\S\\s]*\n")
    #   for (explanation_text in as.list(explanation_texts)) {
    #     print(explanation_text)
    #     explanation_text <- Hmisc::escapeRegex(explanation_text)
    #     rmd_str <- stringr::str_replace(string = rmd_str,
    #                                     pattern = explanation_text,
    #                                     replacement = "\n")
      # }
    # }
  }

  # Save the R markdown document
  print(rmd_output_path)
  fileConn <- file(rmd_output_path, 'w')
  writeChar(rmd_str, fileConn)
  close(fileConn)

  return(rmd_output_path)
}







#' Edit a Rmd Template from xgx
#'
#' \code{edit_rmd_template_str} returns a path to the altered Rmd template
#' 
#'
#' @param data_path Path (as a string) to the dataset that is to be analyzed
#' @param mapping A list of column name mappings from the
#' original (template) dataset column names
#' to the corresponding columns in the new dataset
#' @param author_name The name of the author to be displayed on the template
#' @param multiple_dosing 
#' @param pk_cmt 
#' @param pd_cmt
#' @param pd_data_type 
#' @param rmd_template_path 
#' @param rmd_output_path 
#' @param pdf_output_path 
#' @param html_output_path 
#' @param alter_datetime 
#' @param show_explanation 
#'
#' @return NULL
#'
#' @examples
#'     Name mapping: 
#'     
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @importFrom magrittr "%>%"
#' @export
get_rmd_name <- function(multiple_dosing = FALSE,
                         pk_cmt = NULL,
                         pd_cmt = NULL,
                         pd_data_type = NULL) {

  # # Capitalize to match naming scheme chosen on github repo
  # if (!(is.null(pd_data_type))) {
  #   pd_data_type <- Hmisc::capitalize(pd_data_type)
  # }

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
get_rmd_str <- function(multiple_dosing = FALSE,
                        pk_cmt = NULL,
                        pd_cmt = NULL,
                        pd_data_type = NULL){

  rmd_fname <- get_rmd_name(multiple_dosing = multiple_dosing,
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
    
  }

  return(rmd_str)
}

