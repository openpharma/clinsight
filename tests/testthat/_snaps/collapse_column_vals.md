# collapse_column_vals() can collapse values in a column in a data frame into a string with a given separator, for example a comma. The values to collapse are all the values in a group (defined by the grouping columns). Values excluded from the collapse can also be defined.: produces the expected snapshot

    Code
      collapse_column_vals(df, column = "colname", exclude = "excluded", group_by = "site")
    Output
         ID          colname site
      1   1        collapsed   S3
      2   1        collapsed   S3
      3   1         excluded   S3
      4   1         excluded   S3
      5   1         excluded   S3
      6   2 collapsed, names   S1
      7   2 collapsed, names   S1
      8   2 collapsed, names   S1
      9   3        collapsed   S3
      10  3         excluded   S3
      11  4 collapsed, names   S1
      12  5 collapsed, names   S2
      13  5         excluded   S2
      14  5         excluded   S2
      15  5 collapsed, names   S2

