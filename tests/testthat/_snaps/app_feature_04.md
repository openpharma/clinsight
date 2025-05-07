# Feature 4 | Load data from CSV files, merge with custom metadata, and start the application.As a user, I want to be able to load data from the raw csv file output from the EDC system and use a different metadata file to customize the application.: Scenario 1 - Load raw data with custom metadata. Given raw CSV data exported from the EDC system from subject [9600-002], and with metadata in which total visits is restricted to V0-V10, and with only the study_forms 'Response' and 'Vitals adjusted' in the metadata and with the study name in the metadata settings set to 'Test Study Name'and with, at both Screening and Visit 2 the [Systolic blood pressure] being 99, [Diastolic Blood Pressure] 77, [Pulse] 77, [Resp] 9, and [Temperature] 37.5, and (only at screening) [Weight] 70kg, and that I browse to the 'Common events' tabI expect that I see the timeline of subject [9600-002] with study visits in itand when I browser to the 'Study data' tabI expect that I see the Vital signs page of subject [9600-002]', and that the compact header timeline shows visits all visits available in the dataand that the study name 'Test Study Name' is displayed in the main bar as a header and that I see a figure with the data displayed, and that I see a table with the data displayed after clicking on the table view, and that the data for the figure and table in the app is the same as the raw data in the CSV files.

    Code
      print(fig_data, width = Inf)
    Output
      # A tibble: 12 x 9
         subject_id event_name   event_date item_group      item_name               
         <chr>      <chr>        <date>     <chr>           <fct>                   
       1 9600-002   Screening    2022-01-01 Vitals adjusted Systolic blood pressure 
       2 9600-002   Ext. visit 1 2022-09-11 Vitals adjusted Systolic blood pressure 
       3 9600-002   Screening    2022-01-01 Vitals adjusted Diastolic blood pressure
       4 9600-002   Ext. visit 1 2022-09-11 Vitals adjusted Diastolic blood pressure
       5 9600-002   Screening    2022-01-01 Vitals adjusted Pulse                   
       6 9600-002   Ext. visit 1 2022-09-11 Vitals adjusted Pulse                   
       7 9600-002   Screening    2022-01-01 Vitals adjusted Resp                    
       8 9600-002   Ext. visit 1 2022-09-11 Vitals adjusted Resp                    
       9 9600-002   Screening    2022-01-01 Vitals adjusted Temperature             
      10 9600-002   Ext. visit 1 2022-09-11 Vitals adjusted Temperature             
      11 9600-002   Screening    2022-01-01 Vitals adjusted BMI                     
      12 9600-002   Screening    2022-01-01 Vitals adjusted Weight                  
         item_value item_unit   lower_lim upper_lim
              <dbl> <chr>           <dbl>     <dbl>
       1       99   mmHg             90       160  
       2       99   mmHg             90       160  
       3       77   mmHg             55        90  
       4       77   mmHg             55        90  
       5       77   beats/min        60       100  
       6       77   beats/min        60       100  
       7        9   breaths/min      12        20  
       8        9   breaths/min      12        20  
       9       37.5 °C               35        38.5
      10       37.5 °C               35        38.5
      11       22.1 kg/m2            18.5      30  
      12       70   kg               45       200  

---

    Code
      print(table_data, width = Inf)
    Output
      # A tibble: 2 x 11
        row_review_status subject_id event_name   `Systolic blood pressure`
        <list>            <chr>      <chr>        <chr>                    
      1 <named list [5]>  9600-002   Screening    <b>99*</b> mmHg          
      2 <named list [5]>  9600-002   Ext. visit 1 <b>99*</b> mmHg          
        `Diastolic blood pressure` Pulse                Resp                 
        <chr>                      <chr>                <chr>                
      1 <b>77*</b> mmHg            <b>77*</b> beats/min <b>9*</b> breaths/min
      2 <b>77*</b> mmHg            <b>77*</b> beats/min <b>9*</b> breaths/min
        Temperature     `Weight change since screening` BMI                
        <chr>           <chr>                           <chr>              
      1 <b>37.5*</b> °C <NA>                            <b>22.09*</b> kg/m2
      2 <b>37.5*</b> °C <NA>                            <NA>               
        Weight       
        <chr>        
      1 <b>70*</b> kg
      2 <NA>         

