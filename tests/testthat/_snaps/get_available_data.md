# get_available_data() creates a data frame with all available data per individual. It summarizes the available data points for each individual for each time point. For study data forms, the data points will be taken from event_name. For common forms, the Name column of the pivot table data will be used (for example, the specific adverse event or concomitant medication): Creates the expected data frame with given random appdata input

    Code
      get_available_data(data = appdata, tables = apptables, all_forms = all_forms)
    Output
      # A tibble: 1,869 x 5
         subject_id item_name               item_group     event_name event_label
         <chr>      <chr>                   <chr>          <chr>      <chr>      
       1 BEL_04_772 Hypotension             Adverse events Any visit  Any visit  
       2 BEL_04_772 Atrial Fibrillation     Adverse events Any visit  Any visit  
       3 BEL_04_772 Tachycardia             Adverse events Any visit  Any visit  
       4 BEL_04_772 Urinary Tract Infection Adverse events Any visit  Any visit  
       5 BEL_07_193 Atelectasis             Adverse events Any visit  Any visit  
       6 BEL_08_736 Hypotension             Adverse events Any visit  Any visit  
       7 BEL_08_885 Seizure                 Adverse events Any visit  Any visit  
       8 BEL_08_885 Urinary Incontinence    Adverse events Any visit  Any visit  
       9 BEL_08_885 Urinary Tract Infection Adverse events Any visit  Any visit  
      10 BEL_09_361 Seizure                 Adverse events Any visit  Any visit  
      # i 1,859 more rows

