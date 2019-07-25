#' Check data for various issues
#'
#' \code{xgx_check_data} performs a series of checks on a PK or PKPD dataset
#' It was inspired by the dataset preparation table from 
#' \href{https://iqrtools.intiquan.com/doc/book/analysis-dataset-preparation.html}{IntiQuan}.
#' 
#' The dataset must have the following columns
#' \itemize{
#'   \item ID      = unique subject identifier.  USUBJID is another option if ID is not there
#'   \item EVID    = event ID: 1 for dose, 0 otherwise
#'   \item AMT     = value of the dose
#'   \item TIME    = time of the measurement
#'   \item DV      = dependent value (linear scale).  will check if LIDV or LNDV are also there if DV is not
#'   \item YTYPE   = data measurement for LIDV.  will check if CMT is there, if YTYPE is not
#' }
#'
#' The dataset may also have additional columns
#' \itemize{
#'   \item CENS = flag for censoring of the data because it's below the limit of quantification (BLOQ)
#'   \item MDV  = missing dependent variable - will be counted and then filtered out from the data check
#' }
#'
#' @param data, the dataset to check.  Must contain the above columns
#' @param covariates, the column names of covariates, to explore
#'
#' @return data.frame
#'
#' @examples
#' covariates = c("WEIGHTB","SEX")
#' check      = xgx_check_data(Multiple_Ascending_Dose_Missing_Duplicates,covariates)
#' 
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise_all
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows
#' @importFrom pander panderOptions
#' @importFrom pander pander
#' @importFrom utils head
#' @export
xgx_check_data <- function(data,covariates = NULL){
  #defining column names as variables, because this is a work around CRAN to accept the R package
  #due to the way dplyr and lazy evaluation interacts with the CRAN checking
  #https://stackoverflow.com/q/48750221
  ID <-  EVID <- YTYPE <- MDV <- AMT <- DV <- TIME <- CENS <- 
    Value <- tot <- ntot <- pct <- Data_Check_Issue <- n <- NULL
  
  #check for required column names in dataset  
  if (!("YTYPE" %in% names(data)) && ("CMT" %in% names(data))) {
    warning("Setting YTYPE column equal to CMT\n")
    data$YTYPE <- data$CMT
  }
  if (!("ID" %in% names(data)) && ("USUBJID" %in% names(data))) {
    warning("Setting ID column equal to USUBJID\n")
    data$ID <- data$USUBJID
  }  
  if (!("DV" %in% names(data))) {
    if ("LIDV" %in% names(data)) {
      warning("Setting DV column equal to LIDV\n")
      data$DV <- data$LIDV
    } else if ("LNDV" %in% names(data)) {
      warning("Setting DV column equal to LNDV\n")
      data$DV <- data$LNDV
    }
  }
  if (!("MDV" %in% names(data))) {
    if ("EVID" %in% names(data)) {
      data$MDV <- as.numeric(data$EVID!=0)
      warning("Setting MDV column equal to as.numeric(EVID!=0)\n")
    }
  }
  if (!("CENS" %in% names(data))) {
    warning("Setting CENS column equal to 0\n")
    data$CENS <- 0
  }
  
  required_names <- c("ID","EVID","AMT","TIME","DV","YTYPE")
  missing_cols <- setdiff(required_names,names(data))
  if (length(missing_cols)>0) {
    missing_text <- paste(missing_cols, collapse=",")
    stop(paste0("These columns must be present in the dataset: ",missing_text))
  }
  
  #initialize output tibble
  check <- list()
  data_subset <- list()
  i <- 0 #index for table
  j <- 0 #index for list of data indices
  
  #Number of patients ----
  num_patients <- length(unique(data$ID))
  i <- i+1
  check[[i]] <- tibble::tibble(
    Category    = "Patients",
    Description = "Number of Patients",
    YTYPE       = "-",
    Statistic   = paste0(num_patients),
    Value       = num_patients)
  
  #number of patients with zero observations ----
  zero_obs <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::filter(EVID==0) %>%
    dplyr::count() %>%
    dplyr::filter(n==0)
  num_zero_obs <- nrow(zero_obs)
  
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "MDV",
                      Description = paste0("Number of patients with zero observations"),
                      YTYPE       = "all",
                      Statistic   = paste0(num_zero_obs," ",paste0(zero_obs$ID,collapse = ", ")),
                      Value       = num_zero_obs)
  
  #number of missing data points, to be filtered out from MDV ----
  if ("MDV" %in% names(data)) {
    mdv <- data %>%
      dplyr::group_by(YTYPE) %>%
      dplyr::summarise(n = sum(MDV==1 & EVID==0))
    num_mdv <- sum(mdv$n)
    
    if (num_mdv==0) {
      i <- i+1
      check[[i]] <- tibble::tibble(Category    = "MDV",
                          Description = paste0("Number of Missing Data Points (MDV==1 and EVID==0)"),
                          YTYPE       = "all",
                          Statistic   = "0",
                          Value       = 0)
    } else {
      i <- i+1    
      check[[i]] <- mdv %>%
        dplyr::transmute(Category    = "MDV",
                  Description = paste0("Number of Missing Data Points (MDV==1 and EVID==0)"),
                  YTYPE       = as.character(YTYPE),
                  Statistic   = paste0(n),
                  Value       = n)
      message(paste0("removing ",nrow(num_mdv)," points with MDV==1 & EVID==0 from dataset"))
      data <- dplyr::filter(data,!(MDV==1 & EVID==0))
    }
  }
  
  #number of doses ----
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "Dose",
                      Description = paste0("Number of non-zero doses"),
                      YTYPE       = "-",
                      Value       = sum(data$AMT>0),
                      Statistic   = paste0(Value))
  
  #number of zero doses ----
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "Dose",
                      Description = paste0("Number of zero doses (AMT==0)"),
                      YTYPE       = "-",
                      Value       = sum(data$AMT==0 & data$EVID==1),
                      Statistic   = paste0(Value))
  
  #number of patients that have all zero doses or that never receive any dose ----
  num_doses <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(n = sum(AMT>0))
  
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "Dose",
                      Description = paste0("Number of patients that never received drug"),
                      YTYPE       = "-",
                      Value       = sum(num_doses$n==0),
                      Statistic   = paste0(Value))
  
  #number of data points ----
  num_datapoints <- data %>%
    dplyr::group_by(ID,YTYPE) %>%
    dplyr::count() %>%
    dplyr::group_by(YTYPE) %>%
    dplyr::summarise(tot    = sum(n),
              min    = min(n),
              median = median(n),
              max    = max(n))
  
  i <- i+1
  check[[i]] <- num_datapoints %>%
    dplyr::transmute(Category    = "DV",
              Description = paste0("Number of Data Points"),
              YTYPE       = as.character(YTYPE),
              Statistic   = paste0(tot),
              Value       = tot)
  
  i <- i+1
  check[[i]] <- num_datapoints %>%
    dplyr::transmute(Category    = "DV",
              Description = paste0("Number of Data Points per Individual"),
              YTYPE       = as.character(YTYPE),
              Statistic   = paste0("min = ",min,",  median = ",median,", max = ",max),
              Value       = median)
  
  #check for zero concentrations ----
  num_zero_datapoints <- data %>%
    dplyr::group_by(ID,YTYPE) %>%
    dplyr::group_by(YTYPE) %>%
    dplyr::summarise(tot    = sum(DV==0 & MDV==0, na.rm = TRUE))
  
  i <- i+1
  check[[i]] <- num_zero_datapoints %>%
    dplyr::transmute(Category    = "DV",
              Description = paste0("Number of Data Points with zero value (DV==0)"),
              YTYPE       = as.character(YTYPE),
              Statistic   = paste0(tot),
              Value       = tot)
  j <- j+1
  data_subset[[j]] <- data %>%
    dplyr::filter(DV==0 & MDV==0) %>%
    dplyr::mutate(Data_Check_Issue = "DV == 0")
  
  #check for missing data ----
  num_na_datapoints <- data %>%
    dplyr::group_by(ID,YTYPE) %>%
    dplyr::group_by(YTYPE) %>%
    dplyr::summarise(tot    = sum(is.na(DV) & MDV==0))
  
  i <- i+1
  check[[i]] <- num_na_datapoints %>%
    dplyr::transmute(Category    = "DV",
              Description = paste0("Number of Data Points with NA (is.na(DV))"),
              YTYPE       = as.character(YTYPE),
              Statistic   = paste0(tot),
              Value       = tot)
  
  j <- j+1
  data_subset[[j]] <- data %>%
    dplyr::filter(is.na(DV) & MDV==0) %>%
    dplyr::mutate(Data_Check_Issue = "is.na(DV)")
  
  #check for duplicate data ----
  dup_time <- data %>%
    dplyr::group_by(ID,YTYPE,TIME) %>%
    dplyr::mutate(n = length(DV),
           n = ifelse(n == 1, 0, n)) %>%
    dplyr::ungroup()
  
  i <- i+1
  check[[i]] <- dup_time %>%
    dplyr::group_by(YTYPE) %>%
    dplyr::summarise(ntot = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(Category    = "DV+TIME",
              Description = "Multiple measurements at same time",
              YTYPE       = as.character(YTYPE),
              Statistic   = paste0(ntot),
              Value       = ntot)
  
  j <- j+1
  dup_time <- dup_time %>% 
    dplyr::filter(n>=2)
  data_subset[[j]] <- data %>%
    dplyr::filter(ID      %in% dup_time$ID,
           TIME    %in% dup_time$TIME,
           YTYPE   %in% dup_time$YTYPE) %>%
    dplyr::mutate(Data_Check_Issue = "Duplicate Time Points")
  
  #number of Censored data points ----
  if ("CENS" %in% names(data)) {
    num_cens <- data %>%
      dplyr::group_by(YTYPE) %>%
      dplyr::summarise(tot  = sum(CENS==1))
    num_cens$pct <- round(num_cens$tot/num_datapoints$tot*100)
    
    i <- i+1
    check[[i]] <- num_cens %>%
      dplyr::transmute(Category    = "CENS",
                Description = paste0("Number of Censored Data Points"),
                YTYPE       = as.character(YTYPE),
                Statistic   = paste0(tot, " (",pct,"%)"),
                Value       = tot)
  }
  
  #columns with negative data ----
  neg <- data %>%
    dplyr::select(DV,covariates) %>%
    dplyr::summarise_all( function(x){sum(x<0,na.rm=TRUE)}) 
  nam <- names(neg)
  neg <- neg %>% 
    as.numeric() %>%
    stats::setNames(nam)
  neg <- neg[neg>0]
  
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "All Columns",
                      Description = "Negative Values (number)",
                      YTYPE       = "-",
                      Statistic   = paste0(names(neg),":",neg,collapse = ", "),
                      Value       = sum(neg))
  
  #columns with missing values ----
  na <- data %>%
    dplyr::summarise_all( function(x){sum(is.na(x))}) %>%
    as.numeric() %>%
    stats::setNames(names(data))
  na <- na[na>0]
  
  i <- i+1
  check[[i]] <- tibble::tibble(Category    = "All Columns",
                      Description = "Missing Values (number)",
                      YTYPE       = "-",
                      Statistic   = paste0(names(na),":",na,collapse = ", "),
                      Value       = sum(na))
  missing_summary <- check[[i]]$Statistic
  
  #create summaries ----
  check <- dplyr::bind_rows(check)
  data_subset <- dplyr::bind_rows(data_subset) %>%
    dplyr::select(Data_Check_Issue,ID,TIME,DV,CENS,YTYPE)
  
  #covariates ----
  cov_summary <- xgx_summarize_covariates(data,covariates)
  
  #output
  output <- list(summary         = check,
                cts_covariates  = cov_summary$cts_covariates,
                cat_covariates  = cov_summary$cat_covariates,
                data_subset     = data_subset)
  
  #print the summary ----  
  pander::panderOptions("table.split.table",Inf)
  pander::panderOptions("table.split.cells",60)
  pander::panderOptions("table.alignment.default","left")
  
  cat("\nDATA SUMMARY\n")
  pander::pander(check %>% dplyr::select(-Value))
  
  if (length(output$cts_covariates)>0) {
    cat("CONTINUOUS COVARIATES\n")
    pander::pander(output$cts_covariates)
  } else {
    cat("NO CONTINUOUS COVARIATES\n")
  }
  
  if (length(output$cat_covariates)>0) {
    cat("CATEGORICAL COVARIATES\n")
    pander::panderOptions("table.split.cells",100)
    pander::pander(output$cat_covariates)
  } else {
    cat("NO CATEGORICAL COVARIATES\n")
  }
  
  if (nrow(data_subset)==0) {
  } else if (nrow(data_subset)<=6) {
    cat("POSSIBLE DATA ISSUES IN THE FOLLOWING RECORDS\n")
    pander::pander(data_subset)
  } else {
    cat("POSSIBLE DATA ISSUES - FIRST 6 RECORDS\n")
    pander::pander(utils::head(data_subset))
  }
  cat("The following columns contained missing values\n")
  cat(missing_summary)
  
  return(output)
}

