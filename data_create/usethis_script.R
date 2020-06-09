case1_pkpd = read.csv("data_create/raw/case1_pkpd.csv", stringsAsFactors = FALSE)
usethis::use_data(case1_pkpd, overwrite = TRUE)

mad = read.csv("data_create/raw/mad.csv", stringsAsFactors = FALSE)
usethis::use_data(mad, overwrite = TRUE)

mad_missing_duplicates = read.csv("data_create/raw/mad_missing_duplicates.csv", stringsAsFactors = FALSE)
usethis::use_data(mad_missing_duplicates, overwrite = TRUE)

mad_nca = read.csv("data_create/raw/mad_nca.csv", stringsAsFactors = FALSE)
usethis::use_data(mad_nca, overwrite = TRUE)

nlmixr_theo_sd = read.csv("data_create/raw/nlmixr_theo_sd.csv", stringsAsFactors = FALSE)
usethis::use_data(nlmixr_theo_sd, overwrite = TRUE)

sad = read.csv("data_create/raw/sad.csv", stringsAsFactors = FALSE)
usethis::use_data(sad, overwrite = TRUE)

data_specification_columns = read.csv("data_create/raw/data_specification_columns.csv", stringsAsFactors = FALSE)
usethis::use_data(data_specification_columns, overwrite = TRUE)
