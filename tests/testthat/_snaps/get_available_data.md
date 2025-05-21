# get_available_data() creates a data frame with all available data per individual. It summarizes the available data points for each individual for each time point. For forms with a 'Name' column (mostly common_forms but can also be study data forms) the Name column of the pivot table data will be used (for example, the specific adverse event or concomitant medication). For all other forms, the data points will be taken from event_name.: Creates the expected data frame with given random appdata input

    Code
      get_available_data(data = appdata, tables = apptables, all_forms = all_forms)
    Output
      # A tibble: 1,968 x 6
         subject_id item_name            form_repeat item_group event_name event_label
         <chr>      <chr>                      <int> <chr>      <chr>      <chr>      
       1 BEL_04_772 Hypotension                    1 Adverse e~ Any visit  Any visit  
       2 BEL_04_772 Atrial Fibrillation~           2 Adverse e~ Any visit  Any visit  
       3 BEL_04_772 Tachycardia                    3 Adverse e~ Any visit  Any visit  
       4 BEL_04_772 Urinary Tract Infec~           4 Adverse e~ Any visit  Any visit  
       5 BEL_04_772 Atrial Fibrillation~           5 Adverse e~ Any visit  Any visit  
       6 BEL_07_193 Atelectasis                    1 Adverse e~ Any visit  Any visit  
       7 BEL_08_736 Hypotension                    1 Adverse e~ Any visit  Any visit  
       8 BEL_08_885 Seizure (N: 1)                 1 Adverse e~ Any visit  Any visit  
       9 BEL_08_885 Seizure (N: 3)                 3 Adverse e~ Any visit  Any visit  
      10 BEL_08_885 Urinary Incontinence           2 Adverse e~ Any visit  Any visit  
      # i 1,958 more rows

