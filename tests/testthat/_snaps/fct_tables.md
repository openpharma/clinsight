# Create_table.default() works and creates a table in wide format.: Creates expected table output

    Code
      outcome
    Output
      # A tibble: 4 x 5
        ID    Site  item1     item3      item2
        <chr> <chr> <chr>     <chr>      <chr>
      1 ID1   Site2 16; 34; 1 <NA>       <NA> 
      2 ID2   Site2 47; 29    <NA>       <NA> 
      3 ID2   Site1 <NA>      41; 26; 44 8    
      4 ID1   Site1 <NA>      17         <NA> 

# create_table.continuous() works.: Produces the expected output

    Code
      output
    Output
      # A tibble: 6 x 5
        ID    Site  item1                                 item3         item2         
        <chr> <chr> <chr>                                 <chr>         <chr>         
      1 ID1   Site1 "3 Donkey power"                      <NA>          "1 mL"        
      2 ID3   Site2 "1 mL; missing (my fish was unwell) " 1 Hubble-barn "1 mL"        
      3 ID1   Site2  <NA>                                 <NA>          "missing (my ~
      4 ID2   Site2  <NA>                                 <NA>          "1 mL"        
      5 ID2   Site1  <NA>                                 <NA>          "3 "          
      6 ID3   Site1  <NA>                                 <NA>          "3 "          

# create_table.continuous() works.: Ignores explanation_column and unit_column if they are NULL

    Code
      output
    Output
      # A tibble: 6 x 5
        ID    Site  item1                           item3 item2                      
        <chr> <chr> <chr>                           <chr> <chr>                      
      1 ID1   Site1 "3 "                            <NA>  "1 "                       
      2 ID3   Site2 "1 ; missing (reason unknown) " "1 "  "1 "                       
      3 ID1   Site2  <NA>                           <NA>  "missing (reason unknown) "
      4 ID2   Site2  <NA>                           <NA>  "1 "                       
      5 ID2   Site1  <NA>                           <NA>  "3 "                       
      6 ID3   Site1  <NA>                           <NA>  "3 "                       

# create_table.general() works: creates expected table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 25 x 23
         subject_id Age   Sex    ECOG  Eligible Eligible_Date WHO.classification
         <chr>      <chr> <chr>  <chr> <chr>    <chr>         <chr>             
       1 BEL_08_885 82    Male   1     Yes      2023-07-05    Syndrome S        
       2 BEL_09_464 44    Male   2     Yes      2023-08-14    Syndrome F        
       3 BEL_07_431 42    Male   1     <NA>     <NA>          <NA>              
       4 BEL_09_361 38    Male   0     Yes      2023-07-05    Syndrome G        
       5 DEU_02_968 25    Female 1     Yes      <NA>          Syndrome P        
       6 NLD_06_755 65    Male   1     Yes      2023-07-07    Syndrome W        
       7 NLD_05_282 82    Female <NA>  Yes      <NA>          <NA>              
       8 BEL_09_556 93    Male   <NA>  <NA>     <NA>          <NA>              
       9 DEU_02_866 31    Male   1     Yes      2023-08-08    Syndrome C        
      10 BEL_07_497 50    Female <NA>  Yes      <NA>          <NA>              
      11 BEL_07_193 26    Female 1     Yes      2023-08-23    Syndrome D        
      12 DEU_01_541 74    Male   1     Yes      2023-08-30    Syndrome Y        
      13 NLD_05_561 38    Female 1     Yes      2023-08-28    Syndrome M        
      14 DEU_01_977 59    Female 1     Yes      2023-08-09    Syndrome I        
      15 NLD_06_893 41    Male   0     Yes      2023-09-13    Syndrome R        
      16 BEL_04_133 88    Male   1     Yes      2023-07-06    Syndrome K        
      17 NLD_06_72  82    Female 1     Yes      2023-07-18    Syndrome Z        
      18 NLD_06_959 94    Male   1     Yes      <NA>          Syndrome X        
      19 DEU_02_482 40    Male   1     Yes      2023-07-04    Syndrome T        
      20 DEU_02_387 92    Female 0     Yes      2023-08-02    Syndrome Q        
      21 NLD_03_207 68    Male   1     Yes      2023-07-12    Syndrome E        
      22 BEL_08_45  64    Male   2     Yes      <NA>          Syndrome V        
      23 BEL_04_772 78    Male   0     Yes      2023-08-17    Syndrome O        
      24 BEL_08_736 45    Female 0     Yes      2023-08-17    Syndrome A        
      25 BEL_07_645 46    Male   1     Yes      2023-06-07    Syndrome J        
      # i 16 more variables: WHO.subclassification <chr>, Race <chr>,
      #   ChildbearingPotential <chr>, MenopauseReason <chr>,
      #   DiscontinuationDate <chr>, DiscontinuationReason <chr>,
      #   DisconDeathDate <chr>, DrugAdminDate <chr>, DrugAdminDose <chr>,
      #   DoseModificationDate <chr>, DoseModificationReason <chr>,
      #   DoseModificationNewDose <chr>, DrugDiscontDate <chr>,
      #   DrugDiscontReason <chr>, status <chr>, status_label <chr>

# create_table.adverse_events() works: creates expected AE table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 90 x 18
         subject_id form_repeat Name    AESI  `start date` `end date` `CTCAE severity`
         <chr>            <int> <chr>   <chr> <chr>        <chr>      <chr>           
       1 DEU_02_482           1 Pneumo~ None  2023-07-09   2023-07-18 Grade 3         
       2 NLD_03_207           1 Sepsis  None  2023-08-02   2023-08-14 Grade 3         
       3 BEL_08_885           1 Seizure None  2023-08-11   <NA>       Grade 3         
       4 NLD_03_207           1 Sepsis  None  2023-08-14   2023-08-14 Grade 5         
       5 BEL_07_193           1 Atelec~ None  2023-09-14   <NA>       Grade 3         
       6 BEL_09_361           2 Seizure None  2023-06-13   <NA>       Grade 3         
       7 DEU_02_387           2 Sepsis  None  2023-08-04   2023-08-08 Grade 5         
       8 DEU_02_482           2 Pneumo~ None  2023-08-19   2023-08-19 Grade 5         
       9 BEL_08_885           3 Seizure None  2023-08-29   2023-09-01 Grade 3         
      10 NLD_06_755           6 Pneumo~ None  2023-08-01   <NA>       Grade 3         
      11 BEL_09_361           7 Sepsis  None  2023-08-03   <NA>       Grade 4         
      12 NLD_06_755          22 Stroke  None  2023-08-22   2023-08-24 Grade 4         
      13 NLD_06_755          22 Stroke  None  2023-08-24   2023-08-24 Grade 5         
      14 BEL_09_361           1 Lower ~ None  2023-06-08   2023-06-08 Grade 2         
      15 DEU_02_387           1 Nausea  None  2023-07-07   2023-07-20 Grade 3         
      16 BEL_04_772           1 Hypote~ None  2023-07-07   <NA>       Grade 1         
      17 NLD_06_755           1 Allerg~ None  2023-07-08   2023-07-18 Grade 3         
      18 NLD_05_561           1 Joint ~ None  2023-08-06   2023-08-17 Grade 3         
      19 DEU_02_866           1 Urinar~ None  2023-08-11   <NA>       Grade 1         
      20 DEU_01_541           1 Anemia  None  2023-08-16   <NA>       <NA>            
      21 NLD_06_72            1 Lower ~ None  2023-08-18   <NA>       Grade 1         
      22 DEU_01_977           1 Urinar~ None  2023-08-21   <NA>       Grade 1         
      23 BEL_08_736           1 Hypote~ None  2023-08-22   <NA>       Grade 4         
      24 BEL_04_772           2 Atrial~ None  2023-07-05   <NA>       Grade 2         
      25 NLD_06_755           2 Nausea  None  2023-07-08   2023-08-08 Grade 2         
      # i 65 more rows
      # i 11 more variables: `Treatment related` <chr>, `Treatment action` <chr>,
      #   `Other action` <chr>, `Serious Adverse Event` <chr>,
      #   `SAE Awareness date` <chr>, `SAE Start date` <chr>, `SAE End date` <chr>,
      #   `SAE Category` <chr>, `SAE outcome` <chr>, `SAE Date of death` <chr>,
      #   `SAE Death reason` <chr>

# create_table.medication() works: creates expected medication table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 283 x 10
         subject_id form_repeat Name  Indication Dose  `Start Date` Ongoing `End Date`
         <chr>            <int> <chr> <chr>      <chr> <chr>        <chr>   <chr>     
       1 DEU_01_541           1 Lisi~ Insomnia   300 ~ 2023-NK-NK   Yes     <NA>      
       2 DEU_01_541           2 Oxyc~ Infection  500 ~ 2023-NK-NK   Yes     <NA>      
       3 BEL_07_645           6 Lisi~ Nausea     5 Mi~ 2023-09-13   Yes     <NA>      
       4 BEL_07_645           7 Lisi~ Insomnia   20 M~ 2023-09-13   Yes     <NA>      
       5 DEU_02_866          10 Acet~ Adverse e~ 500 ~ 2023-08-30   Yes     <NA>      
       6 BEL_07_193           9 Hydr~ Adverse e~ 100 ~ 2023-08-30   Yes     <NA>      
       7 BEL_07_193          10 Hydr~ Adverse e~ 8751~ 2023-08-30   Yes     <NA>      
       8 DEU_01_541           3 Pant~ Prophylax~ NA N~ 2023-08-30   Yes     <NA>      
       9 BEL_04_772          15 Pant~ Pain       NA N~ 2023-08-23   Yes     <NA>      
      10 NLD_06_755          33 Para~ Nausea     10 M~ 2023-08-22   Yes     <NA>      
      11 NLD_06_755          34 Meto~ Hypertens~ 20 M~ 2023-08-22   Yes     <NA>      
      12 BEL_04_772          14 Hydr~ Adverse e~ 500 ~ 2023-08-16   Yes     <NA>      
      13 BEL_09_556           1 Pant~ Infection  70 M~ 2023-08-15   Yes     <NA>      
      14 BEL_09_556           2 Lora~ Insomnia   4500~ 2023-08-15   Yes     <NA>      
      15 NLD_06_755          28 Melo~ Prophylax~ 1000~ 2023-08-08   Yes     <NA>      
      16 BEL_08_736           4 Acet~ Infection  5 Mi~ 2023-08-08   Yes     <NA>      
      17 BEL_08_736           5 Meto~ Prophylax~ 20 M~ 2023-08-08   Yes     <NA>      
      18 NLD_06_755           9 Pant~ Adverse e~ 2 Gr~ 2023-08-01   Yes     <NA>      
      19 DEU_02_482          15 Melo~ Infection  NA M~ 2023-07-31   Yes     <NA>      
      20 BEL_09_464           8 Acet~ Pain       1 Mi~ 2023-07-28   Yes     <NA>      
      21 NLD_06_755          29 Cana~ Prophylax~ 1 Un~ 2023-07-26   Yes     <NA>      
      22 NLD_06_755          30 Hydr~ Insomnia   20 M~ 2023-07-23   Yes     <NA>      
      23 BEL_09_361          24 Acet~ Prophylax~ 500 ~ 2023-07-20   Yes     <NA>      
      24 BEL_09_361          25 Melo~ Hypertens~ 10 M~ 2023-07-20   Yes     <NA>      
      25 NLD_06_755          18 Meto~ Pain       1000~ 2023-07-19   Yes     <NA>      
      # i 258 more rows
      # i 2 more variables: `Related Medical History` <chr>, `Related AE` <chr>

# create_table.medical_history() works: creates expected medical history table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 152 x 8
         subject_id form_repeat Name         `Start Date` Ongoing `End Date` Treatment
         <chr>            <int> <chr>        <chr>        <chr>   <chr>      <chr>    
       1 BEL_08_885           1 Epilepsy     2008-01-01   Yes     <NA>       Medicati~
       2 BEL_09_464           1 Atrial Fibr~ 1990-01-01   No      1990-01-01 Surgery  
       3 BEL_09_464           2 Hypothyroid~ 2017-11-01   No      2018-11-01 Medicati~
       4 BEL_09_464           3 Arhtritis    2010-01-01   No      2010-01-01 Surgery  
       5 BEL_09_464           4 Chronic Hep~ 2010-01-01   No      2010-01-01 Surgery  
       6 BEL_09_464           5 Familial Me~ 1990-01-01   Yes     <NA>       Medicati~
       7 BEL_09_464           6 Migraine     2007-01-01   Yes     <NA>       Medicati~
       8 BEL_09_464           7 Atrial Fibr~ 2013-01-01   Yes     <NA>       Medicati~
       9 BEL_09_361           1 Hypertension 2021-02-NK   Yes     <NA>       Medicati~
      10 BEL_09_361           2 Diabetes Me~ 2023-06-NK   Yes     <NA>       Medicati~
      11 BEL_09_361           3 Copd         2003-NK-NK   Yes     <NA>       Medicati~
      12 BEL_09_361           4 Copd         2020-NK-NK   Yes     <NA>       Medicati~
      13 DEU_02_968           1 Hypertension <NA>         <NA>    <NA>       <NA>     
      14 NLD_06_755           1 Familial Me~ 2023-03-15   Yes     <NA>       Non-drug~
      15 NLD_06_755           2 Chronic Hep~ 2023-03-08   Yes     <NA>       Non-drug~
      16 NLD_06_755           3 Familial Me~ 2023-06-28   No      2023-07-05 No treat~
      17 NLD_06_755           4 Hypertension 2023-05-19   Yes     <NA>       No treat~
      18 NLD_06_755           5 Diabetes Me~ 2023-05-19   Yes     <NA>       Surgery  
      19 NLD_06_755           6 Migraine     2023-05-19   Yes     <NA>       No treat~
      20 NLD_06_755           7 Malaria      2015-07-10   No      2015-11-24 Non-drug~
      21 NLD_06_755           8 Malaria      2022-09-12   Yes     <NA>       Medicati~
      22 NLD_06_755           9 Osteoporosis 2015-NK-NK   Yes     <NA>       No treat~
      23 NLD_06_755          10 Malaria      2023-03-08   Yes     <NA>       No treat~
      24 DEU_02_866           1 Epilepsy     2021-01-NK   Yes     <NA>       Medicati~
      25 DEU_02_866           2 Copd         2020-09-NK   Yes     <NA>       Medicati~
      # i 127 more rows
      # i 1 more variable: Comment <chr>

# create_table.conc_procedures() works: creates expected table

    Code
      print(create_table(df, expected_columns = expected_cols), n = 25)
    Output
      # A tibble: 4 x 9
        subject_id form_repeat Name         Indication `Start Date` Ongoing `End Date`
        <chr>            <int> <chr>        <chr>      <chr>        <chr>   <chr>     
      1 NLD_06_755           1 Amputation   Infection  2023-08-04   Yes     <NA>      
      2 DEU_02_866           1 Thrombolysis Hypertens~ 2023-08-18   No      2023-08-18
      3 BEL_07_645           1 Appendectomy Prophylax~ 2019-NK-NK   No      2019-NK-NK
      4 BEL_07_645           2 Amputation   Pain       2019-NK-NK   No      2019-NK-NK
      # i 2 more variables: `Related Medical History` <chr>, `Related AE` <chr>

# create_table.bm_cytology() works: creates expected table

    Code
      print(create_table(df), n = 25)
    Output
      # A tibble: 1 x 8
        subject_id event_name event_repeat event_date `Bone marrow blasts`
        <chr>      <chr>             <dbl> <chr>      <chr>               
      1 885        Screening             1 2023-08-07 91                  
      # i 3 more variables: `BM smear assessment` <chr>, `Auer Rods` <chr>,
      #   `Ringed Sideroblasts` <chr>

