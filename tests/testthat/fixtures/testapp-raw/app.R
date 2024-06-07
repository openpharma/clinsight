pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

raw_data_folder <- "data1pt"
meta_path <- "altered_metadata.xlsx"
# For interactive use: 
# raw_data_folder <- app_sys("tests/testthat/fixtures/testapp-raw/data1pt")
# meta_path <- testthat::test_path("fixtures/testapp-raw/altered_metadata.xlsx")

load_and_run_app <- function(){
  data_folder <- tempdir()
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  meta <- get_metadata(meta_path)
  merged_data <- merge_meta_with_data(
    get_raw_data(
      data_path = raw_data_folder, 
      column_specs = meta$column_specs
      ), 
    meta = meta
    )
  saveRDS(merged_data, file.path(data_folder, "study_data.rds"))
  saveRDS(meta, file.path(data_folder, "metadata.rds"))
  
  run_app(
    data_folder = data_folder,
    test_mode = TRUE, 
    onStart = \(){onStop(\(){unlink(data_folder, recursive = TRUE)})}
  )
}

withr::with_envvar(list("GOLEM_CONFIG_ACTIVE" = "production"), load_and_run_app())
