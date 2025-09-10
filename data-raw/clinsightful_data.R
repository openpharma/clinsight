devtools::load_all(".")

metadata <- get_metadata(filepath = app_sys("metadata.xlsx"))

clinsightful_data <- clinsight::get_raw_csv_data(
  app_sys("raW_data"), 
  synch_time = "2023-09-15 10:10:00 UTC"
) |> 
  merge_meta_with_data(metadata)

# Build a version of `app_data` & app_vars
cs_app_data <- get_appdata(data = clinsightful_data, meta = metadata) 
cs_app_vars <- get_meta_vars(data = cs_app_data, meta = metadata) 

# Build a 'app_tables'
cs_app_tables <- lapply(
  setNames(names(cs_app_data), names(cs_app_data)), \(x){
    create_table(cs_app_data[[x]], expected_columns = names(cs_app_vars$items[[x]]))
  })

# Build a 'available_data'
cs_available_data <- get_available_data(
  data = cs_app_data,
  tables = cs_app_tables,
  all_forms = cs_app_vars$all_forms,
  form_repeat_name = with(
    meta[["table_names"]],
    table_name[raw_name == "form_repeat"]
  ) |>
    tryCatch(error = \(e) "N")
)

usethis::use_data(
  metadata,
  clinsightful_data,
  cs_app_data,
  cs_app_vars,
  cs_app_tables,
  cs_available_data,
  overwrite = TRUE)
