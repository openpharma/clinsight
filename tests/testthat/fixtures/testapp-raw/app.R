pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

datapath <- "data1pt"
# For interactive use: 
# datapath <- app_sys("tests/testthat/fixtures/csvtestdata")

load_and_run_app <- function(){
  data_folder <- tempdir()
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  merged_data <- merge_meta_with_data(
    get_raw_data(
      data_path = datapath, 
      column_specs = metadata$column_specs
      ), 
    meta = metadata
    )
  saveRDS(merged_data, file.path(data_folder, "study_data.rds"))
  saveRDS(metadata, file.path(data_folder, "metadata.rds"))
  
  run_app(
    data_folder = data_folder,
    test_mode = TRUE, 
    onStart = \(){onStop(\(){unlink(data_folder, recursive = TRUE)})}
  )
}

withr::with_envvar(list("GOLEM_CONFIG_ACTIVE" = "production"), load_and_run_app())