#' Multiple Ascending Dose Data Set
#'
#' Model generated PK and PD data to mimic an orally administered small
#' molecule with various endpoints from continuous to ordinal response and
#' count data. Simulated multiple dose administration ranging from 100 mg
#' to 1600 mg, once per day.
#'
#' @format A data frame with the following 19 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab numeric; unique subject ID\cr
#'   column 2: \tab \code{TIME} \tab numeric; time relative to first drug 
#'   administration\cr
#'   column 3: \tab \code{NOMTIME} \tab numeric; nominal time\cr
#'   column 4: \tab \code{TIMEUNIT} \tab character; unit of TIME\cr
#'   column 5: \tab \code{AMT} \tab numeric; dosing amount (for dosing events) 
#'   in mg\cr
#'   column 6: \tab \code{LIDV} \tab numeric; observation on a linear scale 
#'   (observation type determined by CMT), units determined by EVENTU column\cr
#'   column 7: \tab \code{MDV} \tab numeric; TODO\cr
#'   column 8: \tab \code{CMT} \tab integer; compartment number 
#'   (determines observation type):\cr
#'   \tab \tab CMT 1 = Dosing event\cr
#'   \tab \tab CMT 2 = PK concentration\cr
#'   \tab \tab CMT 3 = Continuous response data\cr
#'   \tab \tab CMT 4 = Count response data\cr
#'   \tab \tab CMT 5 = Ordinal response data\cr
#'   \tab \tab CMT 6 = Binary response data\cr
#'   column 9: \tab \code{NAME} \tab character; description of event\cr
#'   column 10: \tab \code{EVENTU} \tab character; unit for observation\cr
#'   column 11: \tab \code{CENS} \tab integer; censored values
#'   (0 = not censored, 1 = censored)\cr
#'   column 12: \tab \code{EVID} \tab integer; event ID (0 = observation, 
#'   1 = dosing event)\cr
#'   column 13: \tab \code{WEIGHTB} \tab numeric; baseline bodyweight (kg)\cr
#'   column 14: \tab \code{SEX} \tab character; sex\cr
#'   column 15: \tab \code{TRTACT} \tab factor; treatment group label\cr
#'   column 16: \tab \code{DOSE} \tab numeric; randomized dose in mg\cr
#'   column 17: \tab \code{PROFDAY} \tab numeric; day of profile\cr
#'   column 18: \tab \code{PROFTIME} \tab numeric; time within PROFDAY\cr
#'   column 19: \tab \code{CYCLE} \tab numeric; count of drug administrations 
#'   received
#' }
"mad"

#' Multiple Ascending Dose Data Set (Duplicates Removed)
#'
#' Model generated PK and PD data to mimic an orally administered small
#' molecule with various endpoints from continuous to ordinal response and
#' count data. Simulated multiple dose administration ranging from 100 mg
#' to 1600 mg, once per day.
#'
#' @format A data frame with the following 19 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab numeric; unique subject ID\cr
#'   column 2: \tab \code{TIME} \tab numeric; time relative to first drug 
#'   administration\cr
#'   column 3: \tab \code{NOMTIME} \tab numeric; nominal time\cr
#'   column 4: \tab \code{TIMEUNIT} \tab character; unit of TIME\cr
#'   column 5: \tab \code{AMT} \tab numeric; dosing amount (for dosing events) 
#'   in mg\cr
#'   column 6: \tab \code{LIDV} \tab numeric; observation on a linear scale 
#'   (observation type determined by CMT), units determined by EVENTU column\cr
#'   column 7: \tab \code{MDV} \tab numeric; TODO\cr
#'   column 8: \tab \code{CMT} \tab integer; compartment number 
#'   (determines observation type):\cr
#'   \tab \tab CMT 1 = Dosing event\cr
#'   \tab \tab CMT 2 = PK concentration\cr
#'   \tab \tab CMT 3 = Continuous response data\cr
#'   \tab \tab CMT 4 = Count response data\cr
#'   \tab \tab CMT 5 = Ordinal response data\cr
#'   \tab \tab CMT 6 = Binary response data\cr
#'   column 9: \tab \code{NAME} \tab character; description of event\cr
#'   column 10: \tab \code{EVENTU} \tab character; unit for observation\cr
#'   column 11: \tab \code{CENS} \tab integer; censored values
#'   (0 = not censored, 1 = censored)\cr
#'   column 12: \tab \code{EVID} \tab integer; event ID (0 = observation, 
#'   1 = dosing event)\cr
#'   column 13: \tab \code{WEIGHTB} \tab numeric; baseline bodyweight (kg)\cr
#'   column 14: \tab \code{SEX} \tab character; sex\cr
#'   column 15: \tab \code{TRTACT} \tab factor; treatment group label\cr
#'   column 16: \tab \code{DOSE} \tab numeric; randomized dose in mg\cr
#'   column 17: \tab \code{PROFDAY} \tab numeric; day of profile\cr
#'   column 18: \tab \code{PROFTIME} \tab numeric; time within PROFDAY\cr
#'   column 19: \tab \code{CYCLE} \tab numeric; count of drug administrations 
#'   received
#' }
"mad_md"

#' Multiple Ascending Dose Data Set (NCA)
#'
#' TODO
#'
#' @format A data frame with the following 7 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab numeric; unique subject ID\cr
#'   column 2: \tab \code{PARAM} \tab character; TODO\cr
#'   column 3: \tab \code{VALUE} \tab numeric; TODO\cr
#'   column 4: \tab \code{DOSE} \tab numeric; randomized dose in mg\cr
#'   column 15: \tab \code{TRTACT} \tab factor; treatment group label\cr
#'   column 14: \tab \code{SEX} \tab character; sex\cr
#'   column 13: \tab \code{WEIGHTB} \tab numeric; baseline bodyweight (kg)
#' }
"mad_nca"

#' Case 1 PKPD Data Set
#'
#' TODO
#'
#' @format A data frame with the following 21 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab integer; unique subject ID\cr
#'   column 2: \tab \code{TIME} \tab numeric; time relative to first drug 
#'   administration\cr
#'   column 3: \tab \code{NOMTIME} \tab numeric; nominal time\cr
#'   column 4: \tab \code{TIMEUNIT} \tab factor; unit of TIME\cr
#'   column 5: \tab \code{AMT} \tab integer; dosing amount (for dosing events) 
#'   in mg\cr
#'   column 6: \tab \code{LIDV} \tab numeric; observation on a linear scale 
#'   (observation type determined by CMT), units determined by EVENTU column\cr
#'   column 7: \tab \code{CMT} \tab integer; compartment number 
#'   (determines observation type):\cr
#'   \tab \tab CMT 1 = Dosing event\cr
#'   \tab \tab CMT 2 = PK concentration\cr
#'   \tab \tab CMT 3 = Continuous response data\cr
#'   \tab \tab CMT 4 = Count response data\cr
#'   \tab \tab CMT 5 = Ordinal response data\cr
#'   \tab \tab CMT 6 = Binary response data\cr
#'   column 8: \tab \code{NAME} \tab factor; description of event\cr
#'   column 9: \tab \code{EVENTU} \tab factor; unit for observation\cr
#'   column 10: \tab \code{CENS} \tab integer; censored values
#'   (0 = not censored, 1 = censored)\cr
#'   column 11: \tab \code{EVID} \tab integer; event ID (0 = observation, 
#'   1 = dosing event)\cr
#'   column 12: \tab \code{WEIGHTB} \tab numeric; baseline bodyweight (kg)\cr
#'   column 13: \tab \code{eff0} \tab numeric; TODO\cr
#'   column 14: \tab \code{TRTACT} \tab factor; treatment group label\cr
#'   column 15: \tab \code{DOSE} \tab integer; TODO\cr
#'   column 16: \tab \code{PROFDAY} \tab integer; day of profile\cr
#'   column 17: \tab \code{PROFTIME} \tab numeric; time within PROFDAY\cr
#'   column 18: \tab \code{CYCLE} \tab integer; count of drug administrations
#'   received\cr
#'   column 19: \tab \code{PART} \tab integer; TODO\cr
#'   column 20: \tab \code{STUDY} \tab integer; TODO\cr
#'   column 21: \tab \code{IPRED} \tab numeric; TODO\cr
#' }
"case1_pkpd"

#' nlmixr Data Set
#'
#' TODO
#'
#' @format A data frame with the following 20 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab integer; TODO\cr
#'   column 2: \tab \code{TIME} \tab numeric; TODO\cr
#'   column 3: \tab \code{AMT} \tab integer; TODO\cr
#'   column 4: \tab \code{RATE} \tab numeric; TODO\cr
#'   column 5: \tab \code{DV} \tab numeric; TODO\cr
#'   column 6: \tab \code{TAD} \tab numeric; TODO\cr
#'   column 7: \tab \code{OCC} \tab integer; TODO\cr
#'   column 8: \tab \code{FLAG} \tab integer; TODO\cr
#'   column 9: \tab \code{MDV} \tab integer; TODO\cr
#'   column 10: \tab \code{EVID} \tab integer; TODO\cr
#'   column 11: \tab \code{WGT} \tab integer; TODO\cr
#'   column 12: \tab \code{BSA} \tab numeric; TODO\cr
#'   column 13: \tab \code{AGE} \tab integer; age in years\cr
#'   column 14: \tab \code{RACE} \tab integer; TODO\cr
#'   column 15: \tab \code{HGT} \tab integer; TODO\cr
#'   column 16: \tab \code{DOS} \tab integer; TODO\cr
#'   column 17: \tab \code{STR} \tab integer; TODO\cr
#'   column 18: \tab \code{INIT} \tab integer; TODO\cr
#'   column 19: \tab \code{CONC} \tab numeric; TODO\cr
#'   column 20: \tab \code{hipoalbuminemia} \tab integer; TODO
#' }
"nlmixr_nimo"

#' NLMIXR Theo SD Data Set
#'
#' TODO
#'
#' @format A data frame with the following 7 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab integer; TODO\cr
#'   column 2: \tab \code{TIME} \tab numeric; TODO\cr
#'   column 3: \tab \code{DV} \tab numeric; TODO\cr
#'   column 4: \tab \code{AMT} \tab numeric; TODO\cr
#'   column 5: \tab \code{EVID} \tab integer; TODO\cr
#'   column 6: \tab \code{CMT} \tab integer; TODO\cr
#'   column 7: \tab \code{WT} \tab numeric; TODO
#' }
"nlmixr_theo_sd"

#' Single Ascending Dose Data Set
#'
#' Model generated PK data to mimic an orally administered small molecule.
#' Simulated single dose administration ranging from 100 mg to 1600 mg.
#'
#' @format A data frame with the following 16 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab numeric; unique subject ID\cr
#'   column 2: \tab \code{TIME} \tab numeric; time relative to first drug 
#'   administration\cr
#'   column 3: \tab \code{NOMTIME} \tab numeric; nominal time\cr
#'   column 4: \tab \code{TIMEUNIT} \tab character; unit of TIME\cr
#'   column 5: \tab \code{AMT} \tab numeric; dosing amount (for dosing events) 
#'   in mg\cr
#'   column 6: \tab \code{LIDV} \tab numeric; observation on a linear scale 
#'   (observation type determined by CMT), units determined by EVENTU column\cr
#'   column 7: \tab \code{MDV} \tab numeric; TODO\cr
#'   column 8: \tab \code{CMT} \tab integer; compartment number 
#'   (determines observation type):\cr
#'   \tab \tab CMT 1 = Dosing event\cr
#'   \tab \tab CMT 2 = PK concentration\cr
#'   column 9: \tab \code{NAME} \tab character; description of event\cr
#'   column 10: \tab \code{EVENTU} \tab character; unit for observation\cr
#'   column 11: \tab \code{CENS} \tab integer; censored values
#'   (0 = not censored, 1 = censored)\cr
#'   column 12: \tab \code{EVID} \tab integer; event ID (0 = observation, 
#'   1 = dosing event)\cr
#'   column 13: \tab \code{WEIGHTB} \tab numeric; baseline bodyweight (kg)\cr
#'   column 14: \tab \code{SEX} \tab character; sex\cr
#'   column 15: \tab \code{TRTACT} \tab factor; treatment group label\cr
#'   column 16: \tab \code{DOSE} \tab numeric; randomized dose in mg 
#'   received
#' }
"sad"

#' Single Ascending Dose PKPD Data Set
#'
#' TODO
#'
#' @format A data frame with the following 16 columns:
#' \tabular{rll}{
#'   column 1: \tab \code{ID} \tab numeric; unique subject ID\cr
#'   column 2: \tab \code{TIME} \tab numeric; TODO\cr
#'   column 3: \tab \code{COBS} \tab numeric; TODO\cr
#'   column 4: \tab \code{EOBS} \tab numeric; TODO\cr
#'   column 5: \tab \code{WEIGHT} \tab numeric; TODO\cr
#'   column 6: \tab \code{AGE} \tab numeric; TODO\cr
#'   column 7: \tab \code{DOSE} \tab integer; TODO\cr
#'   column 14: \tab \code{SEX} \tab character; sex\cr
#'   column 15: \tab \code{RACE} \tab character; TODO\cr
#'   column 16: \tab \code{AMT} \tab integer; TODO
#' }
"sad_pkpd"
