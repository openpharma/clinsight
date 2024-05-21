# db_update(). Feature 1 | As a user, I want to be able to update the database.: Adds a new row to a database if there are new rows

    Code
      dplyr::collect(dplyr::tbl(con, "all_review_data"))
    Output
      # A tibble: 2 x 10
        key_col1  item_group item_name     event_date edit_date_time  reviewed comment
        <chr>     <chr>      <chr>         <chr>      <chr>           <chr>    <chr>  
      1 Test_name Visit 1    Test_item     2023-11-01 2023-11-05 01:~ Yes      ""     
      2 9999      ZZ         new_test_item 1950/01/01 2023-11-12 01:~ No       ""     
      # i 3 more variables: reviewer <chr>, timestamp <chr>, status <chr>

# db_update(). Feature 1 | As a user, I want to be able to update the database.: Adds a new row for each data point with a new/updated EditdateTime.

    Code
      print(dplyr::collect(dplyr::tbl(con, "all_review_data")), width = 10)
    Output
      # A
      #   tibble:
      #   2 x
      #   10
        key_col1
        <chr>   
      1 Test_na~
      2 Test_na~
      # i 9
      #   more
      #   variables:
      #   item_group <chr>,
      #   item_name <chr>,
      #   event_date <chr>,
      #   edit_date_time <chr>, ...

# db_update(). Feature 1 | As a user, I want to be able to update the database.: Does not change the database if there are no changes (latest max edit_date_time is the same)

    Code
      db_update(old_data, db_path = temp_path, common_vars = comvars)
    Output
      [1] "Database up to date. No update needed"
    Code
      dplyr::collect(dplyr::tbl(con, "all_review_data"))
    Output
      # A tibble: 1 x 10
        key_col1  item_group item_name event_date edit_date_time      reviewed comment
        <chr>     <chr>      <chr>     <chr>      <chr>               <chr>    <chr>  
      1 Test_name Visit 1    Test_item 2023-11-01 2023-11-05 01:26:00 Yes      ""     
      # i 3 more variables: reviewer <chr>, timestamp <chr>, status <chr>

