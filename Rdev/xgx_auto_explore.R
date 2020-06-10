#' Plot all functions for the given dataset
#'
#' \code{xgx_ci_summary} returns a PDF document with plots describing the
#' provided dataset
#' 
#' This function can be used quickly explore your data by generating 
#' overview plots before constructing non-linear mixed effects models.
#'
#' @param multiple_dosing This described whether or not the data has multiple
#' time points for which the individual has taken a treatment dose
#'
#' @return NULL
#'
#' @examples
#'     Name mapping: 
#'     
#' 
#' @importFrom stringr str_replace
#' @importFrom readr read_file
#' @importFrom ggplot2 aes
#' @export

library(knitr)
library(stringr)
xgx_auto_explore <- function(data_path = NULL,
                             mapping = NULL,
                             author_name = NULL,
                             multiple_dosing = FALSE,
                             pk_cmt = NULL,
                             pd_cmt = NULL,
                             pd_data_type = NULL,
                             rmd_template_path = NULL,
                             rmd_output_path = NULL,
                             pdf_output_path = NULL,
                             html_output_path = NULL,
                             alter_datetime = TRUE,
                             show_explanation = TRUE) {

  if (is.null(mapping)) { warning("Names need to be mapped
                                  
                                  Examples:
                                  {}
                                  ")
  }
    
  
  # Setup defaults
  file_prefix = "xgx_autoexplore_"
  analysis_fname <- tools::file_path_sans_ext(
                           get_rmd_name(multiple_dosing = multiple_dosing,
                                        pk_cmt = pk_cmt,
                                        pd_cmt = pd_cmt,
                                        pd_data_type = pd_data_type))
  if (is.null(rmd_output_path)) {
    working_dir <- getwd()
    rmd_output_path <- file.path(working_dir, analysis_fname, ".Rmd")
  }
  if (is.null(pdf_output_path)) {
    working_dir <- getwd()
    pdf_output_path <- file.path(working_dir, analysis_fname, ".pdf")
  }
  if (is.null(html_output_path)) {
    working_dir <- getwd()
    html_output_path <- file.path(working_dir, analysis_fname, ".html")
  }

  # A specific file path to an R markdown file can be given; however,
  #   if the template path is not provided, the logic below will decide the 
  #   R markdown template most likely desired from the xgx github.
  #
  #   - Note: The processing / editing of non-xgx Rmd templates will likely
  #             end in strange results.  It is best to use this function with 
  #             xgx Rmd templates
  # 
  if (is.null(rmd_template_path)){
    rmd_str <- get_rmd_str(multiple_dosing = multiple_dosing,
                           pk_cmt = pk_cmt,
                           pd_cmt = pd_cmt,
                           pd_data_type = pd_data_type)
  } else{
    rmd_str <- readr::read_file(rmd_template_path)
  }

  # 2) Edit the Rmd template and your data to fit the standard dataset type
  rmd_ouput_path <- edit_rmd_template_str(rmd_str = rmd_str,
                                          rmd_output_path = rmd_output_path,
                                          data_path = data_path,
                                          author_name = author_name,
                                          alter_datetime = alter_datetime)

  # 3) Run and save the analysis

  # Render and save the HTML document
  rmarkdown::render(input = rmd_output_path,
                    output_file = html_output_path,
                    output_format = "html_document")
  
  # Render and save the PDF
  rmarkdown::render(input = rmd_output_path,
                    output_file = pdf_output_path,
                    output_format = "pdf_document")
}







# Edit Rmd Template
edit_rmd_template_str <- function(rmd_str = NULL,
                                  mapping = NULL,
                                  rmd_output_path = NULL,
                                  data_path = NULL,
                                  author_name = NULL,
                                  alter_datetime = TRUE) {

  std_data_cols = list(
    "Single_Ascending_Dose_Dataset2" = c("ID" ,"TIME" ,"NOMTIME" ,"TIMEUNIT" ,
                                         "AMT" ,"LIDV" ,"CMT", "NAME" ,"EVENTU",
                                         "CENS" ,"EVID" ,"WEIGHTB" ,"SEX" ,
                                         "TRTACT" ,"DOSE"),
    "Multiple_Ascending_Dose_Dataset2" = c("ID" ,"TIME" ,"NOMTIME" ,"TIMEUNIT" ,
                                           "AMT" ,"LIDV" ,"CMT" , "NAME" ,"EVENTU",
                                           "CENS" ,"EVID" , "WEIGHTB" ,"SEX" ,
                                           "TRTACT" ,"DOSE",
                                           "PROFDAY" ,"PROFTIME" ,"CYCLE"),
    "AE_xgx" =     c("SUBJID" ,"DAY","time","Dose","AUC","Cmax","Cmin","Cave",
                     "AUCDAY1","AUCAVE","AETOXGRS","AETOXGRDN","AE"),
    "AUC_Safety" = c("SUBJID" ,"time_hr" ,"AUC_day" ,"AUC_popPK",
                     "Cmax_popPK" ,"Cmin_popPK" ,"Cave_popPK"),
    "dzz_PKConc" = c("SUBJID" ,"ARM" ,"VISIT" ,"PCDTC" ,"TMTPT" ,"RESN" ,"RESU"),
    "mt12345" =    c("ID" ,"TIME" ,"TIM" ,"NTIM" ,"TAD" ,"AMT" ,"DOSE" ,
                     "LIDV", "LNDV" ,"EVID" ,"MDV",
                     "CMT","UNIT" ,"TRTTXT" ,
                     "RNDDOSE" ,"NT" ,"CENS"),
    "PPtmp_NCA" = c("SUBJID" ,"ARM" ,"WNLPARM" ,"PPORRESN" ,"PPORRESU"),
    
    "Oncology_Efficacy_Data" = c("IDSHORT" ,"BOR" ,"BPCHG" ,"OR" ,"BORNUM" ,
                                 "psld" ,"DOSE_ABC" ,"DOSE_DEF" ,"DOSE_combo" ,
                                 "binary_BOR" ,"PR_rate" ,"n" ,"count_cr_pr" ,
                                 "TIME" ,"COMB" ,"TIME_OR" ,"auc0_24"),
    "Oncology_Efficacy_Dose" = c("IDSHORT" ,"DOSE" ,"TIME" ,"COMB")
  )

  user_data <- read.csv(data_path)
  author_name_re <- 'author: \\"(.*)\\"'

  # 1) Alter the path to the data, to match the user given path
  if (!is.null(data_path)) {
  
    # Regular Expressions to extract / insert user alterations into Rmd template
    read_csv_re <- 'read.csv\\(\\"'
    prefix_path_re <- "\\.\\.\\/Data\\/"
    read_csv_extension_re <- '\\.csv\\"\\)'
    orig_data_re <- paste0(read_csv_re,
                           prefix_path_re,
                           '(.*)',
                           read_csv_extension_re)
    
    # Extract the filename (w/o extansion) for the original dataset used within
    #   the Rmd template. This is used to find the column names from that dataset.
    orig_data_name <- stringr::str_match(string = rmd_str,
                                         pattern = orig_data_re)[2]

    ############################
    # Alter user dataset columns
    ############################
    # # While we know the original dataset columns,
    # #   we can change the new data to match
    # 
    # # Get the typical columns used for this dataset
    # # orig_data_cols <- std_data_cols[[orig_data_name]]
    # orig_data_cols <- names(mapping)
    # new_data_cols <- mapping
    # 
    # 
    # # Map names given by user
    # # user_data <- user_data %>% rename_at(vars(names(user_data)),
    # #                                      ~ orig_data_cols)
    #   
    # user_data <- user_data %>% rename_at(vars(new_data_cols),
    #                                        ~ orig_data_cols)
    # 
    # # Save the newly formatted user data
    # 
    # new_data_path <- paste0(data_path, ".renamed_cols")
    # write.csv(user_data, file = new_data_path)



    # Alter dataset in Rmd
    #   - Warning: `mutate` is assumed to be present within an Rmd template

    # Find first location of `mutate``function
    mutate_loc <- stringr::str_locate(rmd_str, "mutate\\(")[1, "start"]

    # Get the string starting at this location
    mutate_start_rmd_str <- substr(rmd_str, start = mutate_loc, stop = nchar(rmd_str))


    # Regular Expressions do not work here, becuase comments can contain parentheses
    # So we will get the `mutate` full expression by parsing
    mutate_expr <- parse(text = mutate_start_rmd_str, n = 1)
    mutate_expr_str <- getParseData(mutate_expr, includeText = TRUE)[1, "text"]

    orig_cols <- names(eval(parse(text = stringr::str_replace(string = mutate_expr_str,
                                      pattern = "mutate",
                                      replacement = "c"))))

    # All of the columns from the rmd string (orig_cols) should be present in map

    # Add key/value pairs that are the same in each dataset
    same_cols <- intersection(names(user_data), orig_cols)
    full_mapping <- mapping
    for (col in same_cols) {
      full_mapping[[col]] = col
    }

    # Create new string from mapping
    new_mutate_str <- ""
    for(old_col in names(full_mapping)){
      new_col<-full_mapping[old_col]
      temp_map <- paste("\n", old_col, "=", new_col)
      if (length(temp_map) == 0) {
        new_mutate_str <- temp_map
      } else {
        new_mutate_str <- paste(new_mutate_str, temp_map)
      }
    }

    # New Rmd
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = Hmisc::escapeRegex(mutate_expr_str),
                                    replacement = new_mutate_string)
    ############################


    # Edit the Rmd string to contain the csv desired filepath
    user_data_path_replacement_re = paste0("\\1", '\\"',
                                           Hmisc::escapeRegex(new_data_path),
                                           '\\"\\)')
    rmd_str <- stringr::str_replace(string = rmd_str,
                                    pattern = "(read.csv\\()(.*)",
                                    replacement = user_data_path_replacement_re)
  }


  # Add authorname
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
  
  # Add mapping at mutate in rmd
  # if (!is.null())
  

  # Create a default Rmd path to save the Rmd file, if one is not provided
  if (is.null(rmd_output_path)) {
    working_dir <- getwd()
    rmd_output_path <- file.path(working_dir, "xgx_autoexplore_markdown.Rmd")
  }
  print(rmd_output_path)
  
  # Save the R markdown document
  fileConn <- file(rmd_output_path)
  writeChar(rmd_str, fileConn)
  close(fileConn)

  return(rmd_output_path)
}







get_rmd_name <- function(multiple_dosing = FALSE,
                         pk_cmt = FALSE,
                         pd_cmt = FALSE,
                         pd_data_type = NULL) {

  # # Capitalize to match naming scheme chosen on github repo
  # if (!(is.null(pd_data_type))) {
  #   pd_data_type <- Hmisc::capitalize(pd_data_type)
  # }

  # Construct the filename via the standard xgx rmd template filename format
  if (!is.null(pk_cmt) & !is.null(pd_cmt)) {
    pk_str = "PKPD"
    pd_str = ""
  }
  else {
    pk_str <- if (!is.null(pk_cmt)) "PK" else ""
    pd_str <- if (!is.null(pd_cmt)) "PD" else ""
  }
  if (multiple_dosing) {
    multiple_dosing_str <- "Multiple_Ascending"
  }
  else{
    multiple_dosing_str <- "Single_Ascending"
  }

  if (!is.null(pd_data_type)){
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
                        pk_cmt = FALSE,
                        pd_cmt = FALSE,
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

