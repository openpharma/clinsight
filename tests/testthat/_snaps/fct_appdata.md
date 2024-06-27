# get_raw_data works: Produces the expected output.

    Code
      get_raw_data(data_path, column_specs = metadata$column_specs)
    Output
      # A tibble: 3,267 x 15
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       2 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       3 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       4 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       5 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       6 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       7 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       8 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       9 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
      10 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
      # i 3,257 more rows
      # i 8 more variables: form_repeat <int>, var <chr>, item_value <chr>,
      #   edit_date_time <dttm>, day <drtn>, vis_day <dbl>, vis_num <dbl>,
      #   event_label <chr>

# merge_meta_with_data. Feature 1 | As a user, I want to be able to merge raw data with metadata. Furthermore, I want to be able to fix suffixes and rename the limits and significance values to the standard names used in the app.: Scenario 1. Given a data frame with raw data,I expect that the output will be the same as recorded in a snapshot.

    Code
      df[c(1, 1000, 2000, 3000, 4000, 5000), ]
    Output
      # A tibble: 6 x 23
        site_code subject_id event_repeat event_id event_name event_date form_id
        <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
      1 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
      2 <NA>      <NA>                 NA <NA>     <NA>       NA         <NA>   
      3 <NA>      <NA>                 NA <NA>     <NA>       NA         <NA>   
      4 <NA>      <NA>                 NA <NA>     <NA>       NA         <NA>   
      5 <NA>      <NA>                 NA <NA>     <NA>       NA         <NA>   
      6 <NA>      <NA>                 NA <NA>     <NA>       NA         <NA>   
      # i 16 more variables: form_repeat <int>, edit_date_time <dttm>, day <drtn>,
      #   vis_day <dbl>, vis_num <dbl>, event_label <chr>, item_name <chr>,
      #   item_type <chr>, item_group <chr>, item_value <chr>, item_unit <chr>,
      #   lower_lim <dbl>, upper_lim <dbl>, significance <chr>, reason_notdone <chr>,
      #   region <chr>

---

    Code
      df
    Output
      # A tibble: 543 x 23
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       2 9600      9600-001              1 SCR      Screening  2022-11-09 DM     
       3 9600      9600-002              1 SCR      Screening  2022-01-01 DM     
       4 9600      9600-002              1 SCR      Screening  2022-01-01 DM     
       5 9600      9600-002              1 SCR      Screening  2022-01-01 DM     
       6 9600      9600-002              1 SCR      Screening  2022-01-01 STE    
       7 9600      9600-002              1 SCR      Screening  2022-01-01 STE    
       8 9600      9600-002              1 SCR      Screening  2022-01-01 VS     
       9 9600      9600-002              1 SCR      Screening  2022-01-01 VS     
      10 9600      9600-002              1 SCR      Screening  2022-01-01 VS     
      # i 533 more rows
      # i 16 more variables: form_repeat <int>, edit_date_time <dttm>, day <drtn>,
      #   vis_day <dbl>, vis_num <dbl>, event_label <chr>, item_name <chr>,
      #   item_type <chr>, item_group <chr>, item_value <chr>, item_unit <chr>,
      #   lower_lim <dbl>, upper_lim <dbl>, significance <chr>, reason_notdone <chr>,
      #   region <chr>

# get_appdata works: produces the expected output

    Code
      get_appdata(clinsightful_data, metadata)
    Output
      $`Adverse events`
      # A tibble: 799 x 24
         site_code subject_id event_repeat event_id  event_name event_date form_id
         <chr>     <chr>             <int> <chr>     <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       2 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       3 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       4 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       5 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       6 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       7 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       8 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
       9 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
      10 Site 08   BEL_08_885            1 COMMON_AE Any visit  2023-08-15 AE     
      # i 789 more rows
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $`CBC regular`
      # A tibble: 381 x 27
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       4 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       5 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       6 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBHM   
       7 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 LBHM   
       8 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 LBHM   
       9 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 LBHM   
      10 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 LBHM   
      # i 371 more rows
      # i 20 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_value <dbl>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <fct>, reason_notdone <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $`Conc. Procedures`
      # A tibble: 28 x 24
         site_code subject_id event_repeat event_id  event_name event_date form_id
         <chr>     <chr>             <int> <chr>     <chr>      <date>     <chr>  
       1 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       2 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       3 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       4 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       5 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       6 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       7 Site 06   NLD_06_755            1 COMMON_PR Any visit  2023-08-08 CP     
       8 Site 02   DEU_02_866            1 COMMON_PR Any visit  2023-08-23 CP     
       9 Site 02   DEU_02_866            1 COMMON_PR Any visit  2023-08-23 CP     
      10 Site 02   DEU_02_866            1 COMMON_PR Any visit  2023-08-23 CP     
      # i 18 more rows
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $Electrolytes
      # A tibble: 275 x 27
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       4 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       5 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       6 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       7 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       8 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       9 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
      10 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
      # i 265 more rows
      # i 20 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_value <dbl>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <fct>, reason_notdone <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $General
      # A tibble: 261 x 24
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 DM     
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 DM     
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 ECOG   
       4 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 EX     
       5 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 EX     
       6 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 STE    
       7 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 STE    
       8 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 WHO    
       9 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 DM     
      10 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 DM     
      # i 251 more rows
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $`Liver function`
      # A tibble: 180 x 27
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       4 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       5 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       6 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       7 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       8 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       9 Site 09   BEL_09_464            1 VIS      Visit 1    2023-08-30 LBSER  
      10 Site 09   BEL_09_464            1 VIS      Visit 1    2023-08-30 LBSER  
      # i 170 more rows
      # i 20 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_value <dbl>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <fct>, reason_notdone <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $`Medical History`
      # A tibble: 757 x 24
         site_code subject_id event_repeat event_id  event_name event_date form_id
         <chr>     <chr>             <int> <chr>     <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 COMMON_MH Any visit  2023-08-24 MH     
       2 Site 08   BEL_08_885            1 COMMON_MH Any visit  2023-08-24 MH     
       3 Site 08   BEL_08_885            1 COMMON_MH Any visit  2023-08-24 MH     
       4 Site 08   BEL_08_885            1 COMMON_MH Any visit  2023-08-24 MH     
       5 Site 08   BEL_08_885            1 COMMON_MH Any visit  2023-08-24 MH     
       6 Site 09   BEL_09_464            1 COMMON_MH Any visit  2023-08-31 MH     
       7 Site 09   BEL_09_464            1 COMMON_MH Any visit  2023-08-31 MH     
       8 Site 09   BEL_09_464            1 COMMON_MH Any visit  2023-08-31 MH     
       9 Site 09   BEL_09_464            1 COMMON_MH Any visit  2023-08-31 MH     
      10 Site 09   BEL_09_464            1 COMMON_MH Any visit  2023-08-31 MH     
      # i 747 more rows
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $Medication
      # A tibble: 3,183 x 24
         site_code subject_id event_repeat event_id  event_name event_date form_id
         <chr>     <chr>             <int> <chr>     <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       2 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       3 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       4 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       5 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       6 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       7 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       8 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
       9 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
      10 Site 08   BEL_08_885            1 COMMON_CM Any visit  2023-08-24 CM     
      # i 3,173 more rows
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $`Renal function`
      # A tibble: 135 x 27
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 LBSER  
       4 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       5 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       6 Site 09   BEL_09_464            1 SCR      Screening  2023-06-05 LBSER  
       7 Site 09   BEL_09_464            1 VIS      Visit 1    2023-08-30 LBSER  
       8 Site 09   BEL_09_464            1 VIS      Visit 1    2023-08-30 LBSER  
       9 Site 09   BEL_09_464            1 VIS      Visit 1    2023-08-30 LBSER  
      10 Site 09   BEL_09_361            1 SCR      Screening  2023-06-05 LBSER  
      # i 125 more rows
      # i 20 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_value <dbl>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <fct>, reason_notdone <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      
      $Response
      # A tibble: 6 x 24
        site_code subject_id event_repeat event_id event_name event_date form_id
        <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
      1 Site 04   BEL_04_133            5 VIS      Visit 5    2023-08-30 RS     
      2 Site 04   BEL_04_133            5 VIS      Visit 5    2023-08-30 RS     
      3 Site 06   NLD_06_72             5 VIS      Visit 5    2023-09-06 RS     
      4 Site 06   NLD_06_72             5 VIS      Visit 5    2023-09-06 RS     
      5 Site 04   BEL_04_772            5 VIS      Visit 5    2023-08-30 RS     
      6 Site 04   BEL_04_772            5 VIS      Visit 5    2023-08-30 RS     
      # i 17 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <chr>, item_type <chr>,
      #   item_group <chr>, item_value <chr>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <chr>, reason_notdone <chr>
      
      $`Vital signs`
      # A tibble: 478 x 27
         site_code subject_id event_repeat event_id event_name event_date form_id
         <chr>     <chr>             <int> <chr>    <chr>      <date>     <chr>  
       1 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       2 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       3 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       4 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       5 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       6 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       7 Site 08   BEL_08_885            1 SCR      Screening  2023-06-05 VS     
       8 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 VS     
       9 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 VS     
      10 Site 08   BEL_08_885            1 VIS      Visit 1    2023-08-09 VS     
      # i 468 more rows
      # i 20 more variables: form_repeat <int>, edit_date_time <dttm>,
      #   db_update_time <dttm>, region <chr>, day <drtn>, vis_day <dbl>,
      #   vis_num <dbl>, event_label <chr>, item_name <fct>, item_type <chr>,
      #   item_group <chr>, item_value <dbl>, item_unit <chr>, lower_lim <dbl>,
      #   upper_lim <dbl>, significance <fct>, reason_notdone <chr>,
      #   value_scaled <dbl>, out_of_lim <fct>, text_label <chr>
      

