# get_available_data() creates a data frame with all available data per individual. It summarizes the available data points for each individual for each time point. For forms with a 'Name' column (mostly common_forms but can also be study data forms) the Name column of the pivot table data will be used (for example, the specific adverse event or concomitant medication). For all other forms, the data points will be taken from event_name.: Creates the expected data frame with given random appdata input

    Code
      dplyr::as_tibble(get_available_data(data = appdata))
    Output
      # A tibble: 2,235 x 7
         subject_id item_name form_repeat item_group event_name event_label event_date
         <chr>      <chr>           <int> <chr>      <chr>      <fct>       <date>    
       1 BEL_04_772 Hypotens~           1 Adverse e~ Any visit  <NA>        2023-08-31
       2 BEL_04_772 Atrial F~           2 Adverse e~ Any visit  <NA>        2023-08-31
       3 BEL_04_772 Tachycar~           3 Adverse e~ Any visit  <NA>        2023-08-31
       4 BEL_04_772 Urinary ~           4 Adverse e~ Any visit  <NA>        2023-08-31
       5 BEL_04_772 Atrial F~           5 Adverse e~ Any visit  <NA>        2023-08-31
       6 BEL_07_193 Atelecta~           1 Adverse e~ Any visit  <NA>        2023-09-14
       7 BEL_08_736 Hypotens~           1 Adverse e~ Any visit  <NA>        2023-09-13
       8 BEL_08_885 Seizure ~           1 Adverse e~ Any visit  <NA>        2023-08-15
       9 BEL_08_885 Urinary ~           2 Adverse e~ Any visit  <NA>        2023-09-06
      10 BEL_08_885 Seizure ~           3 Adverse e~ Any visit  <NA>        2023-09-06
      # i 2,225 more rows

