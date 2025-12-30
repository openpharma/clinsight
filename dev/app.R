
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

# Build a version of `app_data` & app_vars
app_data <- get_appdata(data = merged_data, meta = metadata) 
app_vars <- get_meta_vars(data = app_data, meta = metadata) 

# Build a 'app_tables'
# app_tables <- lapply(
#   setNames(names(app_data), names(app_data)), \(x){
#     create_table(app_data[[x]], expected_columns = names(app_vars$items[[x]]))
#   })

# Build a 'available_data'
available_data <- get_available_data(
  data = app_data,
  # tables = app_tables,            # outdated arg
  # all_forms = app_vars$all_forms, # outdated arg
  form_repeat_name = with(
    meta[["table_names"]],
    table_name[raw_name == "form_repeat"]
  ) |>
    tryCatch(error = \(e) "N")
)

# For timeline data
timeline_data <- get_timeline_data(
  app_data,
  available_data = available_data,
  treatment_label = metadata$settings$treatment_label %||% "\U1F48A T\U2093"
)

# tempdir not useful for production mode
data_folder <- "."
# data_path <- file.path(data_folder, 
#                        "merged_data.rds")
# saveRDS(merged_data, data_path)
# Current saves both RDS and Parquet for data frames for continuity purposes
save_objs <- c(
  "metadata",
  "app_data",
  "app_vars",
  # "app_tables",
  "available_data",
  "timeline_data")
purrr::walk(save_objs, function(x){
  rds_file <- file.path(data_folder, paste0(x, ".rds"))
  saveRDS(get(x), rds_file)
  if(inherits(get(x), "data.frame")) {
    pq_file <- file.path(data_folder, paste0(x, ".parquet"))
    arrow::write_parquet(get(x), pq_file)
  }
})

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
  data = data_folder, # merged_data, # or db_path works too
  # user_db = db_path, # defaults to "user_db.sqlite"
  # onStart = \(){onStop(\(){unlink(data_folder, recursive = TRUE)})} # be careful here
)
