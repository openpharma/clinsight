#' Get raw data
#'
#'
#' @param data_path Path to the folder that contains the CSV files with the
#'   application data.
#' @param column_specs A data frame containing at least the columns 'name_raw',
#'   'name_new', and 'col_type'. Used to set the column specifications, and to
#'   set the column names to the names expected for the application to function.
#' @param delim Delimiter to use to read in files.
#' @param skip Number of rows to skip when reading in files.
#'
#' @return A data frame with raw application data.
#' @export
#' 
get_raw_data <- function(
    data_path,
    column_specs = metadata$column_specs,
    delim = ",",
    skip = 1
){
  all_files <- list.files(data_path, pattern = ".csv")
  if(length(all_files) == 0) stop("No files found. Verify whether the path is correct.")
  all_files <- all_files[!grepl("README.csv$", all_files)]
  
  stopifnot(is.data.frame(column_specs))
  stopifnot(c("name_raw", "name_new", "col_type") %in% names(column_specs))
  stopifnot(required_col_names %in% column_specs$name_new)
  
  col_specs <- setNames(column_specs$col_type, column_specs$name_raw) |> 
    append(list(".default" = vroom::col_skip())) |> 
    vroom::as.col_spec()
  
  raw_data <- vroom::vroom(
    file.path(data_path, all_files),  
    delim = delim, 
    skip = skip, 
    # Consider to make all character columns character type in the future (more robust: 
    # if any is missing, it will be filled with char values using add_missing_cols
    col_types = col_specs, 
    show_col_types = FALSE
  ) |> 
    dplyr::rename(
      setNames(column_specs$name_raw, column_specs$name_new)
      ) |> 
    dplyr::mutate(
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
        form_id %in% c("AE", "CM", "CP", "MH", "MH", "MHTR", "PR") ~ "Any visit",
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
  if(any(grepl("^Other ", raw_data$event_name))) warning(
    "Undefined Events detected. Please verify data before proceeding."
  )
  raw_data
}

#' Merge metadata with raw data
#'
#' Study-specific function that will combine raw data gathered with
#' [get_raw_data()] with study-specific metadata. It also fixes the metadata
#' suffix if needed, and renames the limits and significance values to the app
#' standard names. Some study-specific variables need to be created with this
#' step.
#'
#' @param data A data set with raw study data.
#' @param meta A list of data frames containing metadata.
#' @param expected_columns A character vector with the columns that should be
#'   expected in the data frame. If missing, these columns will be added with
#'   missing data (thus, will be made explicitly missing).
#'
#' @return A data frame.
#' @export
#' 
merge_meta_with_data <- function(
    data,
    meta,
    expected_columns = c("LBORNR_Lower", "LBORNR_Upper", "LBORRESU", 
                         "LBORRESUOTH", "LBREASND", "unit", 
                         "lower_limit", "upper_limit", "LBCLSIG")
){
  stopifnot(is.data.frame(data))
  stopifnot(inherits(meta, "list"))
  missing_colnames <- with(meta$column_specs, name_new[!name_new %in% names(data)]) |> 
    paste0(collapse = ", ")
  if(nchar(missing_colnames) > 0) stop(
    paste0("The following columns are defined in the metadata ", 
           "(column_specs$name_new) but are missing in the study data:\n", 
           missing_colnames, ".")
    )
  merged_data <- data |> 
    # fix MC values before merging:
    fix_multiple_choice_vars(expected_vars = meta$items_expanded$var) |> 
    dplyr::right_join(meta$items_expanded, by = "var") |> 
    dplyr::filter(!is.na(item_value)) |> 
    dplyr::mutate(
      suffix = ifelse(item_name == "ECG interpretation", "LBCLSIG", suffix),
      suffix = ifelse(is.na(suffix), "VAL", suffix),
      # TODO: improve code below to handle exceptions in a more general manner
      suffix = ifelse(suffix %in% c("LBORRES", "VSORRES", "EGORRES") | 
                        item_group %in% c("Cytogenetics", "General"), 
                      "VAL", suffix)
    ) |> 
    dplyr::select(-var) |> 
    dplyr::mutate(
      edit_date_time = max(edit_date_time, na.rm = TRUE), 
      .by = c(subject_id, item_name, event_name, event_repeat)
    ) |> 
    tidyr::pivot_wider(names_from = suffix, values_from = item_value) |> 
    add_missing_columns(expected_columns) |> 
    dplyr::mutate(
      LBORNR_Lower = as.numeric(ifelse(!is.na(lower_limit), lower_limit, LBORNR_Lower)),
      LBORNR_Upper = as.numeric(ifelse(!is.na(upper_limit), upper_limit, LBORNR_Upper)),
      LBORRESU     = ifelse(is.na(LBORRESU), unit, LBORRESU),
      LBORRESU     = ifelse(LBORRESU == "Other", LBORRESUOTH, LBORRESU),
      LBORRESU     = ifelse(is.na(LBORRESU), "(unit missing)", LBORRESU)
    ) |> 
    dplyr::mutate(day = event_date - min(event_date, na.rm = TRUE), .by = subject_id) |> 
    dplyr::select(-c(lower_limit, upper_limit, unit, LBORRESUOTH)) |> 
    dplyr::rename(
      "lower_lim" = LBORNR_Lower,
      "upper_lim" = LBORNR_Upper,
      "item_unit" = LBORRESU,
      "significance" = LBCLSIG,
      "item_value" = VAL,
      "reason_notdone" = LBREASND
    )
  apply_study_specific_fixes(merged_data) 
}


#' Apply study-specific fixes
#'
#' These changes are probably study-specific and need to be changed accordingly.
#'
#' @param data A data frame (for example, raw data merged).
#' @param form_id_vars A character vector with the names of the columns that
#'   identify a form.
#' @return A data frame.
#' 
apply_study_specific_fixes <- function(
    data, 
    form_id_vars = c("subject_id", "event_name", "item_group")
){
  ## apply study-specific fixes:
  # fix significance in ECG before proceeding (stored in its own separate variable):
  ECG_significance <- data |> 
    dplyr::filter(item_name == "ECG interpretation") |> 
    dplyr::mutate(
      significance = dplyr::case_match(
        significance,
        "Normal"        ~ "within limits",
        "Abnormal, NCS" ~ "NCS",
        "Abnormal, CS"  ~ "CS"
      )
    ) |> 
    dplyr::select(dplyr::all_of(c(form_id_vars, "form_repeat", "significance")))
  data <- data |> 
    dplyr::filter(item_name != "ECG interpretation") |> 
    dplyr::rows_update(ECG_significance, by = c(form_id_vars, "form_repeat"))
  
  ## Add weight change:
  weight_change_data <- data |> 
    dplyr::filter(item_group == "Vital signs", item_name == "Weight") |> 
    get_base_value(c("item_name" = "Weight")) |> 
    dplyr::mutate(
      base_weight = as.numeric(base_weight),
      item_value = as.character(round(100*(as.numeric(item_value) - base_weight)/base_weight, 1)),
      upper_lim = 10,
      lower_lim = -10,
      item_name = "Weight change since screening",
      item_unit = "%",
    ) |> 
    dplyr::select(-base_weight)
  data <- dplyr::bind_rows(data, weight_change_data)
  
  # Rename dichotomic value that is only in the database if the answer is 'Yes'.
  data <- data |> 
    dplyr::mutate(
      item_value = ifelse(
        item_group == "Medication" & item_name == "CM MDS/AML Specific", 
        "Yes", 
        item_value
      ),
      .by = c(subject_id, form_repeat)
    ) 
  
  # Add regions: 
  data |> 
    dplyr::mutate(
      region = dplyr::case_when(
        grepl("^AU", site_code)  ~ "AUS",
        grepl("^DE", site_code)  ~ "GER",
        grepl("^FR", site_code)  ~ "FRA",
        TRUE                    ~ NA_character_
      )
    )
}

#' Get appdata
#'  
#' Converts data to a list of data frames in the expected format to be used by the 
#' the Shiny application.
#'
#' @param data A data frame, commonly raw data merged with metadata.
#' @param meta A data frame with metadata. Used to convert the data set to the 
#' appropriate format.
#'
#' @return A list of data frames.
#' @export
#'
get_appdata <-  function(
    data,
    meta = metadata
){
  tableclasses <- gsub("create_table.", "", as.character(utils::methods("create_table")))
  var_levels <- dplyr::distinct(meta$items_expanded, item_name, item_group) 
  
  data <- split(data, ~item_group)
  ## Apply changes specific for continuous data:
  appdata <- lapply(data, \(x){
    if(nrow(x) == 0) return(x)
    item_group_x <- unique(x$item_group)
    if(length(item_group_x) != 1) stop(
      "item_group consists of multipe elements which is not allowed: ", 
      item_group_x
    )
    tableclass <- simplify_string(item_group_x)
    if(tableclass %in% tableclasses){
      class(x) <- c(tableclass, class(x)) 
    }
    if(!all(x$item_type == "continuous")) return(x)
    df <- x |> 
      dplyr::mutate(
        # to preserve the order of the variables in the metadata file:
        item_name = factor(
          item_name, 
          levels = with(var_levels, item_name[item_group == item_group_x])
        )
      ) |> 
      # using group_by for imputing upper limit with max value if needed
      dplyr::group_by(site_code, item_name) |> 
      dplyr::mutate(
        item_value = as.numeric(item_value),
        upper_lim = ifelse(is.na(upper_lim) & !is.na(lower_lim), pmax(1.5*max(lower_lim), max(item_value, na.rm = TRUE)), upper_lim),
        lower_lim = ifelse(is.na(lower_lim) & !is.na(upper_lim), 0, lower_lim),
        value_scaled = ifelse(lower_lim == 0 & upper_lim == 0, item_value, (item_value-lower_lim)/(upper_lim-lower_lim)),
        out_of_lim = ifelse(item_value>upper_lim | item_value<lower_lim, 1, 0),
        significance = dplyr::case_when(
          significance == "NCS"                   ~ "out of limits, clinically insignificant",
          significance == "CS"                    ~ "out of limits, clinically significant",
          is.na(out_of_lim) & is.na(significance) ~ "limits unknown",
          out_of_lim == 0                         ~ "within limits",
          is.na(significance) & out_of_lim == 1   ~ "out of limits, significance unknown",
          TRUE   ~ significance
        ),
        out_of_lim = factor(out_of_lim), # for vital signs figures
        significance = factor(significance, levels = names(col_palette)),
        text_label = paste0(
          "<b>", subject_id, "</b>",
          "\n",
          event_date,
          "\n",
          event_name, " (day ",
          day, ")",
          "\nValue: ",
          round(item_value, 2),
          " ",
          item_unit
        )
      ) |> 
      dplyr::ungroup() 
    class(df) <- c("continuous", class(df))
    df
  }) 
  appdata
}


