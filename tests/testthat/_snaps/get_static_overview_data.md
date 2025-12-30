# get_static_overview_data() works. : creates the expected output

    Code
      get_static_overview_data(data = appdata, available_data,
        expected_general_columns = metadata$general$item_name)
    Output
      # A tibble: 25 x 24
         subject_id Age   Sex    ECOG  Eligible Eligible_Date WHO.classification
         <chr>      <chr> <chr>  <chr> <chr>    <chr>         <chr>             
       1 BEL_04_133 88    Male   1     Yes      2023-07-06    Syndrome K        
       2 BEL_04_772 78    Male   0     Yes      2023-08-17    Syndrome O        
       3 BEL_07_193 26    Female 1     Yes      2023-08-23    Syndrome D        
       4 BEL_07_431 42    Male   1     <NA>     <NA>          <NA>              
       5 BEL_07_497 50    Female <NA>  Yes      <NA>          <NA>              
       6 BEL_07_645 46    Male   1     Yes      2023-06-07    Syndrome J        
       7 BEL_08_45  64    Male   2     Yes      <NA>          Syndrome V        
       8 BEL_08_736 45    Female 0     Yes      2023-08-17    Syndrome A        
       9 BEL_08_885 82    Male   1     Yes      2023-07-05    Syndrome S        
      10 BEL_09_361 38    Male   0     Yes      2023-07-05    Syndrome G        
      # i 15 more rows
      # i 17 more variables: WHO.subclassification <chr>, Race <chr>,
      #   ChildbearingPotential <chr>, MenopauseReason <chr>,
      #   DiscontinuationDate <chr>, DiscontinuationReason <chr>,
      #   DisconDeathDate <chr>, DrugAdminDate <chr>, DrugAdminDose <chr>,
      #   DoseModificationDate <chr>, DoseModificationReason <chr>,
      #   DoseModificationNewDose <chr>, DrugDiscontDate <chr>, ...

