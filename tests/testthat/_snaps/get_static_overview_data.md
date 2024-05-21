# get_static_overview_data() works. : creates the expected output

    Code
      get_static_overview_data(data = appdata, expected_general_columns = metadata$
        general$item_name)
    Output
      # A tibble: 25 x 6
         subject_id status                WHO.classification Age   Sex    event_name  
         <chr>      <chr>                 <chr>              <chr> <chr>  <chr>       
       1 BEL_08_885 Enrolled              Syndrome S         82    Male   Screening, ~
       2 BEL_09_464 Enrolled              Syndrome F         44    Male   Screening, ~
       3 BEL_07_431 Unknown               <NA>               42    Male   Screening   
       4 BEL_09_361 Enrolled              Syndrome G         38    Male   Screening, ~
       5 DEU_02_968 Death                 Syndrome P         25    Female Screening   
       6 NLD_06_755 Death                 Syndrome W         65    Male   Screening, ~
       7 NLD_05_282 Screen failure        <NA>               82    Female Screening   
       8 BEL_09_556 Unknown               <NA>               93    Male   Screening   
       9 DEU_02_866 Enrolled              Syndrome C         31    Male   Screening, ~
      10 BEL_07_497 Withdrawal by subject <NA>               50    Female Screening   
      # i 15 more rows

