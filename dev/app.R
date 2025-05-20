
devtools::load_all()
# pkg_name <- "clinsight"
# library(pkg_name, character.only = TRUE)


# datapath <- "data1pt"
datapath <- app_sys("tests/testthat/fixtures/csvtestdata") # For interactive use

metadata <- get_metadata(filepath = app_sys("data-raw/metadata.xlsx"))
my_raw_data <- get_raw_csv_data(data_path = datapath, synch_time = "2024-01-01 00:00:00 UTC")
  # fix_multiple_choice_vars() - metadata not found
  
merged_data <- merge_meta_with_data(
  data = my_raw_data,
  meta = metadata
  )
# tempdir not useful for production mode
data_folder <- "."
data_path <- file.path(data_folder, 
                       "merged_data.rds")
saveRDS(merged_data, data_path)
db_path <- file.path(data_folder, "user_db.sqlite")

# if test_mode == FALSE, you'll need to setup...
# DB_SECRET env var to setup credentials db
usethis::edit_r_environ()
Sys.getenv("DB_SECRET")

# initiate the user db
db_create(get_review_data(merged_data),
          db_path = db_path
          )

run_app(
  data = data_path, #merged_data, # or db_path works too
  # user_db = db_path, # defaults to "user_db.sqlite"
  # onStart = \(){onStop(\(){unlink(data_folder, recursive = TRUE)})} # be careful here
)



if(rlang::is_interactive()){
  raw_data_folder <- app_sys("tests/testthat/fixtures/testapp-raw/data1pt")
  meta_path <- testthat::test_path("fixtures/testapp-raw/altered_metadata.xlsx") 
}

load_and_run_app <- function(){
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "test")
  meta <- get_metadata(meta_path)
  merged_data <- raw_data_folder |> 
    get_raw_csv_data(synch_time = "2024-01-01 00:00:00") |> 
    merge_meta_with_data(meta = meta)
  saveRDS(merged_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(meta, file.path(temp_folder, "metadata.rds"))
  run_app(
    data_folder = temp_folder,
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
    })}
  )
}