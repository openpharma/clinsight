describe(
  paste0("collapse_column_vals() can collapse values in a column in a data frame ", 
  "into a string with a given separator, for example a comma. ", 
  "The values to collapse are all the values in a group (defined by the ", 
  "grouping columns). Values excluded from the collapse can also be defined."), 
  {
   set.seed(2023)
    df <- data.frame(
      ID = sample(1:5, 15, replace = TRUE),
      colname = sample(c("collapsed", "excluded", "names"), 15, replace = TRUE)
    ) |> 
      dplyr::mutate(
        site = sample(c("S1", "S2", "S3"), 1),
        .by = ID
        ) |> 
      dplyr::arrange(ID, factor(colname))
    it("can collapse all the unique values of a character variable 
        in a data frame with a given separator.", {
          expect_true(is.data.frame(
            collapse_column_vals(df, column =  "colname", 
                                 group_by = c("ID", "site"))
          ))
    })
    it("can exclude variables from the collapse", {
      result <- collapse_column_vals(df, column =  "colname", 
                                     exclude = "excluded", 
                                     group_by = c("ID", "site"))
      expect_true(!"collapsed, excluded, names" %in% result$colname)
      expect_true("excluded" %in% result$colname)
      expect_true("names" %in% result$colname)
      expect_true("collapsed" %in% result$colname)
      expect_true("collapsed, names" %in% result$colname)
    })
    
    it("returns an empty data frame if an empty data frame is provided as input", {
      expect_equal(collapse_column_vals(df[0,], column =  "colname", 
                           exclude = "excluded", group_by = "site"), df[0,])
    })
    it("errors if the data frame is missing", {
      expect_error(collapse_column_vals(NA))
    })
    
    it("produces the expected snapshot", {
      set.seed(2023)
      df <- data.frame(
        ID = sample(1:5, 15, replace = TRUE),
        colname = sample(c("collapsed", "excluded", "names"), 15, replace = TRUE)
      ) |> 
        dplyr::mutate(
          site = sample(c("S1", "S2", "S3"), 1),
          .by = ID
        ) |> 
        dplyr::arrange(ID, factor(colname))
      
      expect_snapshot(
        collapse_column_vals(df, column =  "colname", 
                             exclude = "excluded", group_by = "site")
      )
    })
  }
)


