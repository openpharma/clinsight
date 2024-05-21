# Fri Feb  2 17:02:37 2024 ------------------------------
# LSA This is to anonimize raw Viedoc EDC data, so that we can use it as test data 
# and store it within the GIT repo. 
# anonimize_raw <- function(data_path, outdir = NULL){
#   all_files <- list.files(data_path, pattern = ".csv")
#   if(length(all_files) == 0) stop("No files found. Verify whether the path is correct.")
#   all_files <- all_files[!grepl("README.csv$", all_files)]
#   full_paths <- paste(data_path, all_files, sep = "/")
#   dfs <- lapply(full_paths, \(x){
#     df <- vroom::vroom(x, delim = ",", show_col_types = FALSE)
#     df$`Edit by` <- c(
#       df$`Edit by`[1], 
#       sample(
#         c("DM1", "DM2", "DM3", NA_character_), 
#         length(df$`Edit by`)-1, 
#         replace = TRUE
#       )
#     )
#     df$`Site name` <- NULL
#     df$`Subject Id` <- gsub("IME-", "", df$`Subject Id`)
#     df
#   })
#   names(dfs) <- all_files
#   filedir <- if(is.null(outdir)) {
#     paste(data_path, "test", sep = "/")
#   } else { 
#     outdir 
#   }
#   if(!dir.exists(filedir)){dir.create(filedir)}
#   lapply(all_files, \(x){
#     write_path <- paste(filedir, x, sep = "/")
#     readr::write_csv(dfs[[x]], write_path)
#   })
# }

# csvpath <- testthat::test_path("fixtures", "csvtestdata")
# anonimize_raw(csvpath)
# 
# csvpath <- testthat::test_path("fixtures", "testapp-raw", "csvtestdataonepatient")
# anonimize_raw(csvpath)
# 
# verify the outcome:
# data_path <- testthat::test_path("fixtures", "csvtestdata", "test")
# all_files <- list.files(data_path, pattern = ".csv")
# all_files <- all_files[!grepl("README.csv$", all_files)]
# full_paths <- paste(data_path, all_files, sep = "/")
# dfs <- vroom::vroom(full_paths, delim = ",", show_col_types = FALSE)

# 
# vroom::vroom(full_paths[1], delim = ",", show_col_types = FALSE)