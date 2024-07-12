#' Load metadata
#'
#' Loads all sheets of an Excel file in a named list. Used for reading in
#' metadata.
#'
#' @param filepath Filepath to the .xlsx file.
#' @param expand_tab_items Character vector with the names of the tabs of which
#'   the items need to be expanded. If not empty, a new data frame will be
#'   created named 'expanded_items', containing all items in the tabs of
#'   `expand_tab_items`. Will abort if a tab name is provided that does not
#'   exist in the metadata.
#' @param expand_cols Column names containing the columns for expansion. Will be
#'   ignored if the variable `expand_tab_items` is left empty.
#'
#' @return A list with data frames.
#' @export
#' 
get_metadata <- function(
    filepath,
    expand_tab_items = c("common_forms", "study_forms", "general"),
    expand_cols = "suffix"
    
){
  stopifnot(is.character(filepath))
  if(!grepl(".xlsx$", filepath)) stop(
    "currently only .xlsx files are supported as metadata input"
    )
  sheets <- readxl::excel_sheets(filepath)
  sheets <- setNames(sheets, sheets)
  meta <- lapply(sheets, function(x){
    readxl::read_excel(filepath, sheet = x)
  })
  if(length(expand_tab_items[nchar(expand_tab_items) > 0 ] ) == 0) return(meta)
  if("items_expanded" %in% names(meta)) return({
    message("'items_expanded' already present. Expanding items aborted.")
    meta
  })
  missing_tab_items <- expand_tab_items[!expand_tab_items %in% names(meta)]
  if(length(missing_tab_items) > 0) {
    stop_message <- paste0(
      "The following tab names in 'expand_tab_items' were not found", 
      " in the metadata: ", 
      paste0(missing_tab_items, collapse = ", "), 
      ". \nAre all tab names spelled correctly?"
    )
    stop(stop_message)
  }
  
  meta$items_expanded <- meta[expand_tab_items] |> 
    dplyr::bind_rows() |> 
    expand_columns(
      columns = expand_cols,
      separator = ",",
      unite_with = "var",
      remove_cols = FALSE
    )
  
  lapply(setNames(names(meta), names(meta)), \(x){
    if(!x %in% expand_tab_items) return(meta[[x]])
    meta[[x]] |> 
      dplyr::select(-var, -suffix) |> 
      as.data.frame()
  })
}

#' Rename raw data
#'
#' Helper function to rename raw data
#'
#' @param data A data frame with raw study data.
#' @param column_specs A data frame with column specifications. Should have at
#'   least the columns `name_raw`, containing the current column names, and
#'   `name_new`, containing the new column names. `name_new` should contain all
#'   names that are required for ClinSight to function properly
#'   (`required_col_names`).
#'
#' @return A data frame
#'
#' @examples
#'  specs <- metadata$column_specs
#'  specs$name_raw <- names(mtcars)
#'  rename_raw_data(mtcars, column_specs = specs)
#' 
rename_raw_data <- function(
    data, 
    column_specs = metadata$column_specs
){
  stopifnot("[data] should be a data frame" = is.data.frame(data))
  stopifnot("[column_specs] should be a data frame" = is.data.frame(column_specs))
  if(!all(c("name_raw", "name_new") %in% names(column_specs))){
    stop("Expecting the columns 'name_raw' and 'name_new' in [column_specs]")
  }
  missing_colnames <- with(column_specs, name_raw[!name_raw %in% names(data)]) |> 
    paste0(collapse = ", ")
  if(nchar(missing_colnames) > 0) stop(
    paste0("The following columns are missing in the raw data while they are ", 
           "defined in 'name_raw' of column_specs:\n", 
           missing_colnames, ".")
  )
  missing_new_cols <- required_col_names[!required_col_names %in% column_specs$name_new] |> 
    paste0(collapse = ", ")
  if(nchar(missing_new_cols) > 0) stop(
    paste0("The following columns are missing in 'name_new' of column_specs while they ", 
           "are required for ClinSight to run:\n", 
           missing_new_cols, ".")
  )
  data[column_specs$name_raw] |> 
    setNames(column_specs$name_new)
}

#' Add time vars to raw data
#'
#' @param data A data frame
#' @param required_col_names Required column names.
#'
#' @return A data frame, with derivative time and event variables, needed for
#'   ClinSight to function properly.
#' @export
#'
add_timevars_to_data <- function(
    data,
    required_col_names = required_col_names
){
  stopifnot("[data] should be a data frame" = is.data.frame(data))
  if(!is.null(required_col_names)){
    stopifnot("required_col_names should be a character vector" = is.character(required_col_names)) 
    stopifnot("Some of the required column names are missing" = all(required_col_names %in% names(data))) 
  }
  
  df <- data |> 
    dplyr::mutate(
      edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"),
      event_date = as.Date(event_date),
      day = event_date - min(event_date, na.rm = TRUE), 
      vis_day = ifelse(event_id %in% c("SCR", "VIS", "VISEXT", "VISVAR", "FU1", "FU2"), day, NA),
      vis_num = as.numeric(factor(vis_day))-1,
      event_name = dplyr::case_when(
        event_id == "SCR"    ~ "Screening",
        event_id %in% c("VIS", "VISEXT", "VISVAR")    ~ paste0("Visit ", vis_num),
        grepl("^FU[[:digit:]]+", event_id)  ~ paste0("Visit ", vis_num, "(FU)"),
        event_id == "UN"     ~ paste0("Unscheduled visit ", event_repeat),
        event_id == "EOT"    ~ "EoT",
        event_id == "EXIT"   ~ "Exit",
        form_id %in% c("AE", "CM", "CP", "MH", "MH", "MHTR", "PR", "ST", "CMTR", "CMHMA") ~ "Any visit",
        TRUE                ~ paste0("Other (", event_name, ")")
      ),
      event_label = dplyr::case_when(
        !is.na(vis_num)   ~ paste0("V", vis_num),
        event_id == "UN"   ~ paste0("UV", event_repeat),
        TRUE              ~ event_name
      ),
      .by = subject_id
    ) |> 
    dplyr::arrange(
      factor(site_code, levels = order_string(site_code)),
      factor(subject_id, levels = order_string(subject_id))
    )
  if(any(grepl("^Other ", df$event_name))) warning(
    "Undefined Events detected. Please verify data before proceeding."
  )
  df
}



#' Correct multiple choice variables
#'
#' In some EDC systems, if there is a multiple choice variable in which multiple
#' answers are possible, the variable will be renamed with a suffix with the
#' multiple answers in it. For example var1, var2, for answers 1 and 2. This
#' function cleans this specific output so that the variable name remains
#' consistent.
#'
#' @param data A data frame. 
#' @param expected_vars Character vector containing the expected names of the
#'   variables.
#' @param var_column column name in which the variable names are stored
#' @param value_column column name in which the values of the variables are
#'   stored
#' @param suffix Multiple choice suffix. Used to define multiple choice values
#' @param common_vars variables used for identifying unique rows in the dataset.
#' @param collapse_with character value to collapse the multiple choice options
#'   with. If this value is NULL, the rows will be left as is.
#'
#' @return data frame with corrected multiple choice variables
#' @examples
#'  df <- data.frame(
#'   ID = "Subj1",
#'   var = c("Age", paste0("MH_TRT", 1:4)),
#'   item_value = as.character(c(95, 67, 58, 83, 34))
#'  )
#'  fix_multiple_choice_vars(df, common_vars = "ID")
#' @export
#' 
fix_multiple_choice_vars <- function(
    data,
    expected_vars = metadata$items_expanded$var,
    var_column = "var",
    value_column = "item_value",
    suffix = "[[:digit:]]+$",
    common_vars = c("subject_id", "event_repeat", "event_date", "form_repeat"),
    collapse_with = "; "
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(expected_vars))
  stopifnot("var_column should be a vector of length 1" = {
    is.character(var_column) & length(var_column) == 1
    })
  stopifnot("suffix should be a character vector of length 1" = {
    is.character(suffix) & length(suffix) == 1
  })
  stopifnot(is.character(common_vars))
  if(!is.null(collapse_with)){
    stopifnot("collapse_with should be a character vector of length 1" = {
      is.character(collapse_with) & length(collapse_with) == 1
    })
  }
  
  all_vars <- unique(data[[var_column]])
  
  missing_vars <- expected_vars[!expected_vars %in% all_vars]
  if(length(missing_vars) == 0) return(data)
  
  vars_to_adjust <- lapply(
    missing_vars, 
    \(x){all_vars[grep(paste0("^", x, suffix), all_vars)] }
  )
  
  multiple_choice_vars <- missing_vars[sapply(vars_to_adjust, length) != 0]
  if(length(multiple_choice_vars) == 0) return(data)
  cat("multiple choice vars that will be adjusted: ", multiple_choice_vars, sep = "\n")
  data_adjusted <- data |> 
    dplyr::filter(.data[[var_column]] %in% unlist(vars_to_adjust), !is.na(.data[[value_column]])) |> 
    dplyr::arrange(.data[[var_column]]) |> 
    dplyr::mutate(var = gsub(suffix, "", var)) 
  if(!is.null(collapse_with)){
    # Probably redundant since the variables will be collapsed already in the 
    # functions `create_table.xxx`. 
    data_adjusted <- data_adjusted |>   
      dplyr::mutate(
        item_value = paste0(item_value, collapse = collapse_with),
        .by = dplyr::all_of(c(var_column, common_vars))
      )
  }
  # note: Column edit_date_time can still cause multiple rows after step below.
  data_adjusted <- dplyr::distinct(data_adjusted)
  
  df <-  data |> 
    dplyr::filter(!.data[[var_column]] %in% unlist(vars_to_adjust)) |> 
    dplyr::bind_rows(data_adjusted)
  df  
}


#' Get meta variables
#' 
#' Prepares and extracts consistent variable names based on both metadata 
#' and the provided data. To be used in both the UI and server functions 
#' of a Shiny app. 
#'
#' @param data List. Data to use.
#' @param meta List. metadata to use.
#'
#' @return a list with all important names to be used in a clinical trial.
#' @export
#'
get_meta_vars <- function(data = appdata, meta = metadata){
  stopifnot(inherits(data, "list"))
  stopifnot(inherits(meta, "list"))
  if(length(data) == 0) stop("Empty list with data provided")
  vars <- list()
  # add metadata variables:
  vars$events <- setNames(meta$events$event_label, meta$events$event_name)
  
  vars$items <- meta$items_expanded |> 
    dplyr::distinct(item_name, item_group) |> 
    split(~item_group) |> 
    lapply(\(x){setNames(simplify_string(x$item_name), x$item_name)})
  vars$groups <- meta$groups$item_group
  common_forms <- unique(meta$common_forms$item_group)
  vars$all_forms <- data.frame(
    "main_tab" = c(
      rep("Common events", times = length(common_forms)),
      rep("Study data", times = length(vars$groups))
      ),
   "form" = c(common_forms, vars$groups)
  )
  
  # add variables dependent on dataset:
  vars$subject_id <- order_string(get_unique_vars(data, "subject_id")[[1]])
  vars$Sites     <- get_unique_vars(data, c("site_code", "region")) |> 
    dplyr::arrange(factor(site_code, levels = order_string(site_code)))
  vars$table_names <- setNames(meta$table_names$raw_name, meta$table_names$table_name) 
  vars
}

#' Get base value
#' 
#' Adds base value to a long-format data frame.
#'
#' @param data data frame in long format.
#' @param var A named character vector with the variable name to use. 
#' The name of the character vector should match the column in the data frame with the variable names. 
#' @param event A names character vector with the event that identifies the baseline value. 
#' The name of the character vector should match the column in the data frame with the event names. 
#' @param value_column A string containing the column name with the item values.
#' @param id_column The columns identifying a unique participant (subject_id).
#'
#' @return as data frame with an additional column named "base_{varname}". 
#' @export
#' @examples
#'  library(dplyr)
#'  library(tidyr)
#'  # create a long time series data frame:
#'  df <- as.data.frame(EuStockMarkets) |> 
#'   mutate(time = time(EuStockMarkets)) |>
#'   pivot_longer(-time)
#'  output <- get_base_value(df, c("name" = "DAX"), event = c("time" = df$time[1]),
#'                           value_column = "value", id_column = NULL)
#' 
get_base_value <- function(
    data,
    var = c("item_name" = "Weight"),
    event = c("event_name" = "Screening"),
    value_column = "item_value",
    id_column = "subject_id"
){
  stopifnot(is.data.frame(data), is.character(var), 
            inherits(event, c("numeric", "character")), 
            is.character(value_column))
  stopifnot("'var' should be a named character vector" = !is.null(names(var)))
  stopifnot("'event' should be a named character vector"= !is.null(names(event)))
  basevar_colname <- paste0("base_", simplify_string(var))
  
  if(nrow(data) == 0) return({data[[basevar_colname]] <- double(0); data})
  id_column <- if(is.null(id_column)) character(0) else id_column
  keep_cols <- na.omit(c(id_column, value_column))
  df <- data[data[[names(var)]] == var & data[[names(event)]] == event, keep_cols]
  
  if(nrow(df) == 0) return({data[[basevar_colname]] <- NA_real_; data})
  names(df)[length(names(df))] <- basevar_colname
  if(length(id_column) == 0) return({cbind(data, df)})
  dplyr::left_join(data, df, by = id_column)
}


#' Adjust column names 
#' 
#' Small helper function to adjust column names in a data frame. Can be used in 
#' a `magrittr` or native `R` pipe. 
#' 
#' Used in the application in the [create_table()] form-specific S3 methods 
#' to remove common prefixes for better table layout. For example, the prefix 
#' "AE" in front of all column names of the Adverse event table can be removed 
#' with this function.
#' 
#' @param data A data frame
#' @param pattern Pattern to search for in the names.
#' @param replacement Replacement for the pattern. Default to empty (thus, 
#' removing the pattern from the data frame names).
#'
#' @return A data frame with adjusted column names.
#' @export
#'
#' @examples
#' adjust_colnames(head(iris), "^Sepal", "Flower")
adjust_colnames <- function(
    data,
    pattern,
    replacement = ""
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(pattern), is.character(replacement))
  names(data) <- gsub(pattern = pattern, replacement = replacement, names(data))
  data
}

#' Add missing columns
#' 
#' Small helper function. Checks if all expected columns are in the data frame 
#' and if not, adds a column with all missing variables in the data. Thus, the 
#' columns will be made explicitly missing in the data frame. 
#' 
#' @param data A data frame. 
#' @param columns Expected columns in the data frame.
#'
#' @return A data frame with at least all the columns named in `columns`. 
#' The added columns will be of class `character`. 
#' @export
#'
#' @examples
#' add_missing_columns(head(iris), c("important_column1", "important_column2"))
add_missing_columns <- function(
    data,
    columns
){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(columns))
  n_columns_input <- length(columns)
  columns <- columns[!is.na(columns)]
  columns <- columns[!columns == ""]
  if(length(columns) == 0) {
    warning("No non-missing columns defined in `columns`. 
            Returning data frame unaltered.")
    return(data)
  } 
  if(length(columns) < n_columns_input){
    warning(
      paste0(
        n_columns_input - length(columns), 
        " missing or empty strings removed from the 'columns' argument"
      )
    )
  }
  for(i in columns){
    if(!i %in% names(data)){
      data[[i]] <- rep(NA_character_, times = nrow(data))
    }
  }
  data
}

#' Custom interactive datatable
#'
#' Small wrapper around [DT::datatable()]. Will be used to create tables in a
#' consistent style.
#'
#' @param data A data frame to use within the application.
#' @param rename_vars An optional named character vector. If provided, it will
#'   rename any column names found in this vector to the provided name.
#' @param title Optional. Character string with the title of the table.
#' @param selection See [DT::datatable()]. Default set to 'single'. 
#' @param ... Other optional arguments that will be parsed to [DT::datatable()].
#'
#' @return A `DT::datatable` object.
#' @export
#'
#' @examples datatable_custom(mtcars)
datatable_custom <- function(
    data, 
    rename_vars = NULL, 
    title = NULL, 
    selection = "single",
    ...
    ){
  stopifnot(is.data.frame(data))
  if(!is.null(rename_vars)){
    stopifnot(is.character(rename_vars))
    data <- dplyr::rename(data, dplyr::any_of(rename_vars))
  }
  if(!is.null(title)){
    stopifnot(is.character(title))
    title <- tags$caption(
      style = 'caption-side: top; text-align: center;',
      tags$h5(tags$b(title))
    )
  }
  DT::datatable(
    data, 
    selection = "single",
    caption = title,
    ...
  ) 
}
