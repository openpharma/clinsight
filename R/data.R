#' Application Metadata
#'
#' A list of data frames and settings containing metadata that will be used for
#' the application. The metadata will be merged with with raw data. It controls
#' the variables that will be used in the application, and in which tab the
#' variables will be shown. The goal is that most, if not all, study-specific
#' data will be captured in the metadata, leaving the scripts to run the
#' application largely unaltered between studies.See `vignette("Metadata")` for
#' in-depth information.
#'
#' @format ## `metadata`: A list with `r length(metadata)` objects.
#' 
#' ```{r }
#' str(metadata)
#' ```
#'
#' @source Can be created with an Excel file. The Excel file format is chosen so
#'   that the metadata can be changed easily per study. See
#'   `raw-data/metadata.R` for details.
#' 
"metadata"


#' Clinical Trial test data
#'
#' A data.frame containing randomly created clinical trial data. Used for
#' testing purposes. It will also be used to run the app with example data when
#' executing `run_app()` with the default configuration settings, or when
#' running either `test_clinsight()` or `golem::run_dev()`. See also
#' `vignette('Metadata')` and `vignette('clinsight')` for an explanation on how
#' to create custom study data for clinsight.
#'
#' @format a data.frame with `r nrow(clinsightful_data)` rows and
#'   `r ncol(clinsightful_data)` variables.
#'  -   `site_code`: character or integer, identifier for study site.
#'  -   `subject_id`: character, unique identifier for a subject.
#'  -   `event_id`: character, event identifier in raw data. Replaced by
#'   `event_name` and `event_label` within ClinSight. An "event" generally
#'   characterizes some sort of site visit (e.g. "Screening", "Visit 1", "Visit
#'   2"). Note that some `event_id`s track events that could apply outside of
#'   any visit, like AE, ConMed, Medical History, etc.
#'  -   `event_name`: character, the name of an event identifier used within
#'   the application. Created after merging raw data with metadata.
#'  -   `event_label`: character, shorter name of an event identifier. Created
#'   after merging raw data with metadata. Used instead of `event_name` for some
#'   compact visualizations.
#'  -   `event_date`: Date, the date associated with `event_name`.
#'  -   `event_repeat`: integer, keeps track of unique `event_id` for a
#'   single `subject_id` and `event_date`.
#'  -   `form_id`: character, a unique `form_id` identifier. Currently unused;
#'   `item_group` is used instead, which is controlled using the metadata.
#'  -   `form_repeat`: integer, keeps track of unique `item_name`s collected
#'   from a specific form for a given `subject_id`. `form_repeat` is
#'   particularly helpful when consolidating data like Adverse Events into this
#'   data format. Specifically, if more than one AE is collected on a patient,
#'   they'll have more than one `form_repeat`.
#'  -   `edit_date_time`: date-time (`POSIXct`), the last time this record was
#'   edited.
#'  -   `region`: character, describing the region code that `site_code` falls
#'   under.
#'  -   `day`: a `difftime()` R object, measuring the number of days each visit
#'   is from screening.
#'  -   `vis_day`: numeric, a numeric representation of `day`. Created after
#'   merging raw data with metadata.
#'  -   `vis_num`: numeric, a numeric representation of `event_name`. Created
#'   after merging raw data with metadata. Only counted for scheduled events
#'   (can be controlled in the metadata).
#'  -   `item_name`: character, the name of a metric or parameter of interest.
#'  -   `item_type`: character, classifies `item_name`s into either
#'   'continuous' or 'other', where continuous types are those generally
#'   associated with the CDISC "basic data structure" (BDS). That is, each
#'   `item_name` metric is collected over time at a patient visit
#'   (`event_name`). The 'other' type represents all non-time dependent
#'   measures, like demographic info, adverse events, Medications, medical
#'   history, etc.
#'  -   `item_group`: character, defines in which form the items with
#'   `item_name` will appear.
#'  -   `item_value`: character, the measurement collected for a given
#'   `item_name`. The value collected may be a number like 150 (when collecting
#'   a patient's weight) or a word (such as 'white' for the subject's race).
#'  -   `item_unit`: character, tracking the unit of measurement for
#'   `item_name` and `item_value`.
#'  -   `lower_lim`: numeric, some `item_name`s (particularly the 'continuous'
#'   type) have a pre-defined range of values that are considered normal. This
#'   is the lower limit to that range.
#'  -   `upper_lim`: numeric, some `item_name`s (particularly the 'continuous'
#'   type) have a pre-defined range of values that are considered normal. This
#'   is the upper limit to that range.
#'  -   `significance`: character, either 'CS' which means
#'   'Clinically Significant' or 'NCS' which means 'Not Clinically Significant'.
#'  -   `reason_notdone`: character, an effort to describe why the `item_value`
#'   field is `NA` / missing.
#'
#' @source Can be recreated with  `get_metadata(get_raw_csv_data(data_path =
#'   system.file("raw_data", package = "clinsight")))`.
"clinsightful_data"


#' Application Metadata
#'
#' A list of data frames and settings containing metadata that will be used for
#' the application. The metadata will be merged with raw data. It controls
#' the variables that will be used in the application, and in which tab the
#' variables will be shown. The goal is that most, if not all, study-specific
#' data will be captured in the metadata, leaving the scripts to run the
#' application largely unaltered between studies. See `vignette("Metadata")` for
#' in-depth information.
#'
#' @format `metadata`: A list with `r length(metadata)` objects.
#'
#' @source Can be created with an Excel file. The Excel file format is chosen so
#'   that the metadata can be changed easily per study. See
#'   `raw-data/metadata.R` for details.
"metadata"


#' Application Data
#'
#' A list of data frames split by `item_group`. It is currently
#' not needed to provide the application data in this format. However, the
#' function can be useful for clinsight data customization, especially in
#' combination with [create_table()]. It creates table classes of the
#' `item_group` name (in lower case letters, and all punctuation replaced by an
#' underscore), as long as there is a method available with the corresponding
#' item_group name for the function [create_table()]. If all data is of type
#' 'continuous', then laboratory values such as lab limits and units are
#' cleaned.
#'
#' @format ## `cs_app_data`: A list with `r length(cs_app_data)` objects.
#' 
#' ```{r }
#' str(cs_app_data)
#' ```
#'
#' @source See `raw-data/clinsightful_data.R` for details.
#' 
"cs_app_data"


#' Application Variables
#'
#' Named list version of `metadata` object.
#'
#' @format ## `cs_app_vars`: A list with `r length(cs_app_vars)` objects.
#' 
#' ```{r }
#' str(cs_app_vars)
#' ```
#'
#' @source See `raw-data/clinsightful_data.R` for details.
#' 
"cs_app_vars"


#' Application Timeline Data
#'
#' A data.frame containing timeline data.
#'
#' @format ## `cs_timeline_data`: data.frame formatted for consumption by
#'   `timevis` package.
#' 
#' ```{r }
#' str(cs_timeline_data)
#' ```
#'
#' @source See `raw-data/clinsightful_data.R` for details.
#' 
"cs_timeline_data"


#' Application 'Available Data'
#'
#' Data frame containing info about available data per individual,
#' such as visits, adverse events, etc. Will be used in module
#' [mod_queries_server()], to select available items to create a query for per
#' individual and per form. Required columns are the ones distinctively
#' identifying an item. For now that are site_code, event_name, subject_id,
#' event_label, item_group, item_name.
#'
#' @format a data.frame with `r nrow(cs_available_data)` rows and
#'   `r ncol(cs_available_data)` variables.
#' 
#' ```{r }
#' str(cs_available_data)
#' ```
#'
#' @source See `raw-data/clinsightful_data.R` for details.
#' 
"cs_available_data"


#' ClinSight key columns
#'
#' A character string containing the names of the columns that are needed to
#' distinguish unique records. All `ClinSight`-compatible data sets need to at
#' least contain these columns.
#'
#' @format A character vector with the following key column names:
#'   `r key_columns`.
#'   
#' @export
key_columns <- c(
  "subject_id",
  "event_name",
  "item_group",
  "form_repeat",
  "item_name"
)
