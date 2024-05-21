pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

datapath <- "data1pt"
# For interactive use: 
# datapath <- app_sys("tests/testthat/fixtures/csvtestdata")

merged_data <- merge_meta_with_data(get_raw_data(
  data_path = datapath, column_specs = metadata$column_specs))
data_folder <- tempdir()
data_path <- file.path(data_folder, "merged_data.rds")
saveRDS(merged_data, data_path)
db_path <- file.path(data_folder, "testdb.sqlite")

run_app(
  data = data_path, 
  user_db = db_path, 
  test_mode = TRUE, 
  onStart = \(){onStop(\(){unlink(data_folder, recursive = TRUE)})}
)
