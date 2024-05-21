# Feature 4 | Load data from raw csv files. As a user, I want to be able to load data from the raw csv file output from the EDC system, and show this data in the application.: Scenario 1 | Load raw data from Viedoc and display in application from one patient. Given raw CSV data exported from the EDC system from patient [IME-9600-002], with, at both Screening and Visit 2 the [Systolic blood pressure] being 99, [Diastolic Blood Pressure] 77, [Pulse] 77, [Resp] 9, and [Temperature] 37.5, and (only at screening) [Weight] 70kg, and that I browse to the 'Study data' tabI expect that I see the Vital signs page of patient 'IME-9600-002', and that I see a figure with the data displayed, and that I see a table with the data displayed after clicking on the table view, and that the data for the figure and table in the app is the same as the raw data in the CSV files.

    Code
      fig_data
    Output
      # A tibble: 13 x 9
         subject_id event_name event_date item_group  item_name   item_value item_unit
         <chr>      <chr>      <date>     <chr>       <fct>            <dbl> <chr>    
       1 9600-002   Screening  2022-01-01 Vital signs Systolic b~       99   mmHg     
       2 9600-002   Visit 2    2022-09-11 Vital signs Systolic b~       99   mmHg     
       3 9600-002   Screening  2022-01-01 Vital signs Diastolic ~       77   mmHg     
       4 9600-002   Visit 2    2022-09-11 Vital signs Diastolic ~       77   mmHg     
       5 9600-002   Screening  2022-01-01 Vital signs Pulse             77   beats/min
       6 9600-002   Visit 2    2022-09-11 Vital signs Pulse             77   beats/min
       7 9600-002   Screening  2022-01-01 Vital signs Resp               9   breaths/~
       8 9600-002   Visit 2    2022-09-11 Vital signs Resp               9   breaths/~
       9 9600-002   Screening  2022-01-01 Vital signs Temperature       37.5 °C       
      10 9600-002   Visit 2    2022-09-11 Vital signs Temperature       37.5 °C       
      11 9600-002   Screening  2022-01-01 Vital signs Weight cha~        0   %        
      12 9600-002   Screening  2022-01-01 Vital signs BMI               22.1 kg/m2    
      13 9600-002   Screening  2022-01-01 Vital signs Weight            70   kg       
      # i 2 more variables: lower_lim <dbl>, upper_lim <dbl>

---

    Code
      print(table_data, width = Inf)
    Output
      # A tibble: 2 x 9
        event_name `Systolic blood pressure` `Diastolic blood pressure`
        <chr>      <chr>                     <chr>                     
      1 Screening  <b>99*</b> mmHg           <b>77*</b> mmHg           
      2 Visit 2    <b>99*</b> mmHg           <b>77*</b> mmHg           
        Pulse                Resp                  Temperature    
        <chr>                <chr>                 <chr>          
      1 <b>77*</b> beats/min <b>9*</b> breaths/min <b>37.5*</b> °C
      2 <b>77*</b> beats/min <b>9*</b> breaths/min <b>37.5*</b> °C
        `Weight change since screening` BMI                 Weight       
        <chr>                           <chr>               <chr>        
      1 <b>0*</b> %                     <b>22.09*</b> kg/m2 <b>70*</b> kg
      2 <NA>                            <NA>                <NA>         

