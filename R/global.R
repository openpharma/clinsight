
# use script below for development purposes, to load data global environment
# with eval(global)
global <- quote({
  data_local <- file.path(Sys.getenv("DATA_FOLDER", app_sys()), "merged_data.rds")
  raw_data_remote <- Sys.getenv("RAW_DATA_PATH")
  db_path <- file.path(Sys.getenv("DATA_FOLDER", app_sys()), "user_db.sqlite")
  data_synched <- FALSE
  
  if(!file.exists(data_local)){
    warning("No data found. Trying to rebuild raw data from remote source")
    merged_data <- merge_meta_with_data(get_raw_data(
      data_path = raw_data_remote, column_specs = metadata$column_specs))
    cat("saving raw data locally\n")
    saveRDS(merged_data, data_local)
    if(!file.exists(data_local)) stop("Could not save data set locally.")
    data_synched <- TRUE
  }
  merged_data <- readRDS(data_local)
  
  if(!file.exists(db_path)){
    warning("no database found. New database will be created")
    # If needed, load older data here and use settings such as below in 
    # <db_create> for testing purposes:
    # reviewed = "Yes", reviewer = "Medical Monitor 1", status = "old"
    db_create(get_review_data(merged_data), db_path = db_path) 
  } else{
    db_update(get_review_data(merged_data), db_path, data_synched = data_synched) 
  }
  
  appdata <- get_appdata(merged_data)
  vars <- get_meta_vars(data = appdata, meta = metadata)
  apptables <- lapply(
    setNames(names(appdata), names(appdata)), \(x){
      create_table(appdata[[x]], expected_columns = names(vars$items[[x]]))
    })
  check_appdata(appdata, metadata)
})


utils::globalVariables(
  c(
    "appdata",
    "clinsightful_data",
    "app_vars",
    "db_path",
    "metadata",
    "subject_id",
    "edit_date_time",
    "event_date",
    "event_name",
    "event_repeat",
    "form_repeat",
    "event_label",
    "item_group",
    "item_name",
    "item_value",
    "site_code",
    "reviewed",
    "DiscontinuationDate",
    "DiscontinuationReason",
    "status",
    "sex",
    "suffix",
    "timestamp",
    "Age",
    "ECOG",
    "Name",
    "Unit Other",
    "Unit",
    "form",
    "raw_data",
    "query_id",
    "needs_review",
    "reviewed",
    "Sex",
    "var",
    "in_use",
    "lower_lim", 
    "lower_limit",
    "upper_lim", 
    "upper_limit",
    "out_of_lim",
    "n",
    "query",
    "type",
    "resolved",
    "reviewer",
    "CTCAE severity worsening",
    "Eligible", 
    "base_weight",
    "Frequency",
    "Frequency Other", 
    "LBCLSIG", 
    "LBORNR_Lower", 
    "LBORNR_Upper",
    "LBORRESU", 
    "LBORRESUOTH", "LBREASND", 
    "Route", "Route Other", 
    "SAE End date", 
    "SAE Start date", 
    "Serious Adverse Event", 
    "Start Date",
    "VAL", 
    "WHO.classification", 
    "all_data", 
    "apptables",
    "con", 
    "content",
    "day",
    "end date",
    "group", 
    "have_changed",
    "input", 
    "item", 
    "item_unit",
    "must_change", 
    "n_rows", 
    "new_query",
    "review_row",
    "significance", 
    "start date", 
    "style", 
    "unit", 
    "vars", 
    "vis_day",
    "event_id"
  )
)


