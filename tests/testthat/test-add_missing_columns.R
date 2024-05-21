describe(
  "add_missing_columns() makes implicitly missing columns in a 
    dataframe explicitly missing. It adds all columns with columns names  
    in a character vector of choice to the dataset if the column is missing.", 
  {
    it(
      "Returns a data frame with the same number of rows as the input data frame, 
        and adds the columns in the [column] argument to the data frame if 
        these are not yet present, 
        and errors if not a data frame is provided.", 
      {
        expect_true(is.data.frame(add_missing_columns(iris, c("c1", "c2"))))
        expect_equal(names(add_missing_columns(iris, c("c1", "c2"))), 
                     c(names(iris), "c1", "c2"))
        expect_equal(nrow(add_missing_columns(iris, c("c1", "c2"))), 
                     nrow(iris))
        expect_error(add_missing_columns(NA, "c1"))
      }
    )
    it("Returns the input data frame if the columns in the [column] argument are 
        already present in the data frame ", {
          expect_equal(
            add_missing_columns(iris, c("Sepal.Length", "Sepal.Width")),
            iris
          )
        })
    it("Returns the original data frame and gives a warning if 
     all column names are missing (NA_character_)", {
       expect_equal(suppressWarnings(add_missing_columns(iris, NA_character_)), iris)
       expect_warning(
         add_missing_columns(iris, NA_character_), 
         "No non-missing columns defined"
       )
     })
    it("Removes missing values from the `columns` argument and displays a 
       warning when doing so.", {
         expect_equal(
           names(suppressWarnings(add_missing_columns(iris, c(NA, "c1")))), 
           c(names(iris), "c1")
         )
         expect_warning(
           add_missing_columns(iris, c(NA, "", "c1")), 
           "missing or empty strings removed"
         )
       })
    it("Can add a missing column if the provided data frame has zero rows", {
      test_case <- mtcars[0,]
      expect_no_error(add_missing_columns(test_case, "new_column"))
      results <- add_missing_columns(test_case, "new_column")
      expect_equal(nrow(results), 0)
      expect_equal(names(results), c(names(mtcars), "new_column"))
      expect_equal(class(results[["new_column"]]), "character")
    })
  })
