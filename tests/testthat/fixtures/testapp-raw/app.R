pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

raw_data_folder <- "data1pt"
meta_path <- "altered_metadata.xlsx"

if(rlang::is_interactive()){
  raw_data_folder <- app_sys("tests/testthat/fixtures/testapp-raw/data1pt")
  meta_path <- testthat::test_path("fixtures/testapp-raw/altered_metadata.xlsx") 
}

clinsight_meta <- get_metadata(meta_path)
clinsight_data <- raw_data_folder |> 
  get_raw_csv_data(synch_time = "2024-01-01 00:00:00") |> 
  merge_meta_with_data(meta = clinsight_meta) 

test_clinsight(clinsight_data, clinsight_meta)
