# summarize_review_data() works: Scenario 1 - Given a random data set provided, I expect that summary dataframe snapshot will be as expected

    Code
      summarize_review_data(review_df, common_vars = c("subject_id", "Form"),
      event_var = "Event", date_time_vars = "Edit date")
    Output
      # A tibble: 124 x 6
         subject_id Form           Event              `Edit date`      status reviewed
         <chr>      <chr>          <chr>              <chr>            <chr>  <chr>   
       1 BEL_08_885 CBC regular    Screening, Visit 1 2023-08-15 02:0~ new/u~ No      
       2 BEL_08_885 Electrolytes   Screening          2023-08-10 01:1~ new/u~ No      
       3 BEL_08_885 Renal function Screening          2023-09-07 02:5~ new    No      
       4 BEL_08_885 Liver function Screening          2023-08-10 01:1~ new/u~ No      
       5 BEL_08_885 Vital signs    Screening, Visit 1 2023-08-15 01:4~ new/u~ No      
       6 BEL_09_464 CBC regular    Screening, Visit 1 2023-08-30 22:3~ new/u~ No      
       7 BEL_09_464 Electrolytes   Screening, Visit 1 2023-08-30 22:2~ new/u~ No      
       8 BEL_09_464 Renal function Screening, Visit 1 2023-08-30 22:2~ new/u~ No      
       9 BEL_09_464 Liver function Screening, Visit 1 2023-08-30 22:2~ new/u~ No      
      10 BEL_09_464 Vital signs    Screening, Visit 1 2023-08-30 22:0~ new/u~ No      
      # i 114 more rows

