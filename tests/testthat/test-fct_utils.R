
# Function should give the timestamp of the current time.
describe(
  "time_stamp() should give the timestamp of the current time.", 
  {
    it("gives a character value as output that can be converted to a date value.", {
      expect_true(is.character(time_stamp()))
      expect_no_error(as.POSIXct(time_stamp()))
      expect_true(is_date(as.POSIXct(time_stamp())))
    })
    it("errors when format or timezone are not recognized by internal formats 
     (see `OlsonNames()` for timezones and `strptime()` for correct date formats.", {
       expect_error(time_stamp(form = "incorrect format"))
       expect_error(time_stamp(timezone = "incorrect format"))
     })
  })

describe(
  "timestamp() function creates a timestamp of the current time, in the 
  expected format (form) and timezone.", 
  {
    it("Gives a character output that can be converted to a date format without error", {
      expect_true(is.character(time_stamp()))
      expect_no_error(as.POSIXct(time_stamp()))
      expect_true(is_date(as.POSIXct(time_stamp())))
    })
    it("errors if date-time or timezone format is unknown", {
      expect_error(time_stamp(form = "incorrect format"))
      expect_error(time_stamp(timezone = "incorrect format"))
    })
  }
)

describe(
  "get_max_time() gives the maximum date/time in a column provided in a 
  data set or a list of data sets", 
  {
    df_list <- lapply(1:10, \(x){data.frame(
      "edit_date_time" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12)
    )}) 
    # add a controlled max date:
    df_list[[11]] <- data.frame("edit_date_time" = as.Date("2023-10-30"))
    df <- do.call("rbind", df_list)
    
    it("gives expected output if either a list or a data frame is provided as input", {
      expect_equal(as.character(get_max_time(df_list, "edit_date_time")), "2023-10-30")
      expect_equal(as.character(get_max_time(df, "edit_date_time")), "2023-10-30")
      expect_true(is_date(get_max_time(df_list, "edit_date_time")))
    })
    it("errors if a data frame with zero rows is provided and if all values are 
     missing (which might happen if the wrong date format is provided)", {
       expect_error(get_max_time(df[0,, drop = FALSE], "edit_date_time"))
       df_NA <- df; df_NA$edit_date_time <- NA
       expect_error(get_max_time(df_NA, "edit_date_time"))  
     })
  }
)


describe(
  "expand_columns() will expand one or more list columns in a 
  data frame into additional rows.", 
  {
    df <- head(iris, n = 6) |> 
      dplyr::mutate(expansion_1 = list("feature.1, feature.2"), 
                    expansion_2 = list("group.1, group.2"))
    it("can expand one column and gives an error if the provided column name 
       does not exist", {
         expect_true(is.data.frame(expand_columns(df, "expansion_1"))) 
         expect_equal(nrow(expand_columns(df, "expansion_1")), 12)
         expect_true(all(unique(expand_columns(df, "expansion_1")$expansion_1) %in% 
                           c("feature.1", "feature.2")))
         expect_error(expand_columns(df, "non_existing_column"))
       })
    it("can unite expansions with existing column. The new name is the existing 
       name united with the expansion column value, with _ separator in between.", {
         df$Species_longname <- df$Species
         expanded_df <- expand_columns(df, "expansion_1", unite_with = "Species_longname")
         expect_true(is.data.frame(expanded_df))
         expect_true(all(
           expanded_df$Species_longname == 
             paste(expanded_df$Species, expanded_df$expansion_1, sep = "_")
         ))
       })
    it("can remove expansion columns if needed", {
      expanded_df <- expand_columns(df, "expansion_1", unite_with = "Species", 
                                    remove_cols = TRUE)
      expect_true(!"expansion_1" %in% names(expanded_df))
      
    })
    it("can expand multiple columns", {
      expect_true(is.data.frame(expand_columns(df, c("expansion_1", "expansion_2")))) 
      expect_equal(nrow(expand_columns(df, c("expansion_1", "expansion_2"))), 24)
      expect_true(all(
        unique(expand_columns(df, c("expansion_1", "expansion_2"))$expansion_1) %in% 
          c("feature.1", "feature.2")
      ))
      expect_true(all(
        unique(expand_columns(df, c("expansion_1", "expansion_2"))$expansion_2) %in% 
          c("group.1", "group.2")
      ))
    })
  }
)

describe(
  "title_case() works", 
  {
    test_case <- "this will be converted to upper case"
    expected_result <- "This Will Be Converted To Upper Case"
    it("gives the expected title case output", {
      expect_true(is.character(title_case(test_case)))
      expect_equal(title_case(test_case), expected_result)
      
    })
    it("works on character vectors and returns NA if a 
       value in the vector is missing", {
         expect_equal(
           title_case(c("convert me", "convert me too", NA)), 
           c("Convert Me", "Convert Me Too", NA_character_)
         )
       })
  }
)

describe(
  "create_unique_id() creates a unique id every time 
  the function is called", 
  {
    it("gives a character value as output", {
      expect_true(is.character(create_unique_id(5)))
    })
    it("produces unique ids every time it is called", {
      expect_true(create_unique_id(5) != create_unique_id(5))
    })
  }
)

describe(
  "simplify_string() works", 
  {
    custom_names <- c("  Custom_name.  to Clean.#$", "#$another   complicated. Name")
    cleaned_names <- c("custom_name_to_clean", "another_complicated_name")
    it("gives a character output with expected clean names", {
      expect_true(is.character(simplify_string(custom_names)))
      expect_equal(simplify_string(custom_names), cleaned_names)
    })
    it("errors if something else than a vector is provided", {
      expect_error(simplify_string(as.data.frame(custom_names)))
      expect_error(simplify_string(list(custom_names)))
    })
    it("converts factors to character strings without error", {
      factor_names <- factor(c("$@To.  be{Converted", "!*To bE.Simplified"))
      expected_names <- c("to_be_converted", "to_be_simplified")
      expect_equal(simplify_string(factor_names), expected_names)
    })
  }
)

describe(
  "order_string() works", 
  {
    test_cases <- c("participant_023_4", "participant_023_3", NA, 
                    "_participant_01_", "first_participant_980-")
    expected_results <- c(NA, "first_participant_980-", "_participant_01_", 
                          "participant_023_3", "participant_023_4")
    
    it("returns a character vector, with missing values first, 
       following with strings ordered first by letters and then by numbers", {
         expect_true(is.character(order_string(test_cases)))
         expect_equal(order_string(test_cases), expected_results)
       })
    it("errors if input is not a vector", {
      expect_error(order_string(mtcars))
    })
    it("warns if input is not of class character, but still gives an output", {
      expect_warning(order_string(c(TRUE, FALSE)))
      expect_true(is.character(suppressWarnings(order_string(c(TRUE, FALSE)))))
    })
  }
)


describe(
  "get_unique_vars() works", 
  {
    test_case <- list(
      data.frame(
        "Sites" = c("site_1", "site_3", "site_4"),
        "ID" = c("subj_1", "subj_5", "subj_8")
      ),
      data.frame(
        "Sites" = c("site_1", "site_3", "site_9"),
        "ID" = c("subj_7", "subj_9", NA_character_)
      )
    )
    expected_results <- data.frame(
      "Sites" = c("site_1", "site_3", "site_4", "site_1", "site_3", "site_9"),
      "ID"    = c("subj_1", "subj_5", "subj_8", "subj_7", "subj_9", NA_character_)
    )
    it("gets all unique variables, outputs a data frame, and returns the 
       expected results", {
         expect_true(is.data.frame(get_unique_vars(test_case, "ID")))
         expect_true(is.data.frame(get_unique_vars(test_case, c("Sites", "ID"))))
         expect_equal(get_unique_vars(test_case, c("Sites", "ID")), expected_results)
       })
    it("returns a data frame with with the requested variables but with missing values 
       if the variables are not available in the dataset", {
         expect_equal(
           get_unique_vars(test_case, "missing_var"),
           data.frame(missing_var = NA_character_)
         )
         outcome <- get_unique_vars(test_case, c("Sites", "missing_var"))
         rownames(outcome) <- NULL
         outcome_expected <- data.frame(
           Sites =  c("site_1", "site_3", "site_4", "site_9"),
           missing_var = rep(NA_character_, times = 4)
         )
         expect_equal(outcome, outcome_expected)
       })
    it("works with a data frame as input", {
      test_df <- dplyr::bind_rows(test_case)
      expect_equal(
        get_unique_vars(test_df, c("Sites", "ID")),
        expected_results
      )
    })
    it("handles missing var variables in a data frame", {
      test_df <- dplyr::bind_rows(test_case)
      expect_equal(
        get_unique_vars(test_df, c("Sites", "ID", "missing_var")),
        cbind(expected_results, "missing_var" = NA_character_)
      )
    })
    it("errors if all provided variable names in 'var' have zero characters", {
      expect_error(get_unique_vars(test_case, ""), "No variable names provided")
    })
  }
)

describe(
  "bind_rows_custom() works",
  {
    test_case <- list(
      data.frame(
        "ID" = c("subj_1", "subj_5", "subj_8"),
        "values" = c("1", "4", "7")
      ),
      data.frame(
        "ID" = c("subj_7", "subj_9", NA_character_),
        "values" = c(10, 12, 5)
      )
    )
    expected_results <- data.frame(
      "ID" = c("subj_1", "subj_5", "subj_8", "subj_7", "subj_9", NA_character_),
      "values" = c("1", "4", "7", "10", "12", "5")
    )
    it("errors if the input is not a list of data frames", {
      expect_error(bind_rows_custom(mtcars), "input should be a list with data frames")
    })
    it("returns a data frame with expected results", {
      expect_true(is.data.frame(bind_rows_custom(test_case, "values")))
      expect_equal(bind_rows_custom(test_case, "values"), expected_results)
    })
    it("does not error with an empty data frame in the list", {
      test_case <- list(
        data.frame("ID" = character(0), "values" = character(0)
        ),
        data.frame(
          "ID" = c("subj_7", "subj_9", NA_character_),
          "values" = c(10, 12, 5)
        )
      )
      expected_results <- test_case[[2]]
      expected_results$values <- as.character(expected_results$values)
      expect_true(is.data.frame(bind_rows_custom(test_case, "values")))
      expect_equal(bind_rows_custom(test_case, "values"), expected_results)
    })
    it("returns an empty data frame with column names preserved when all data 
       frames in the list are empty", {
         test_case <- list(
           data.frame("ID" = character(0), "values" = character(0)),
           data.frame("ID" = character(0), "values" = character(0))
         )
         expected_results <- test_case[[1]]
         expect_equal(bind_rows_custom(test_case, "values"), expected_results)
       })
    it("returns an empty data frame with column names preserved when all data 
       frames in the list are empty, AND the columns that need conversion are of different type", {
         test_case <- list(
           data.frame("ID" = character(0), "values" = double(0)),
           data.frame("ID" = character(0), "values" = character(0))
         )
         expected_results <- test_case[[2]]
         expect_equal(bind_rows_custom(test_case, "values"), expected_results)
       })
    it("does not error with if the column to be converted does not exist in one 
       of the data frames. Instead the column will be added for that data frame 
       with missing character values.", {
         test_case <- list(
           data.frame("ID" = "subj_x"),
           data.frame("ID" = "subj_7", "values" = 10)
         )
         expected_results <- data.frame(
           ID = c("subj_x", "subj_7"),
           values = c(NA_character_, "10")
         )
         expect_true(is.data.frame(bind_rows_custom(test_case, "values")))
         expect_equal(bind_rows_custom(test_case, "values"), expected_results)
       })
  }
)

describe(
  "collapse_fct_levels() combines factor levels.", 
  {  
    fct <- collapse_fct_levels(x = LETTERS, new_levels = list("A-C" = c("A", "B", "C")))
    it("gives a factor as output with the expected levels and length", {
      expect_true(inherits(fct, "factor"))
      expect_equal(levels(fct), c("A-C", LETTERS[-c(1:3)]) )
      expect_equal(length(fct), 26)
    })
    it("errors if not a factor or character vector is provided", {
      expect_error(collapse_fct_levels(NULL))
      expect_error(collapse_fct_levels(c(1,3,5)))
    })
    it("warns if an empty vector is provided", {
      expect_warning(collapse_fct_levels(""))
    })
    it("warns for non-existent levels if warn_non_existent_levels is TRUE", {
      expect_warning(
        collapse_fct_levels(x = LETTERS, 
                            new_levels = list("A-C" = c("A", "B", "wrong-name")), 
                            warn_non_existent_levels = TRUE)
      )
    })
    it("returns an empty factor with new_levels as levels if a 
       vector with only missing values is provided", {
         expect_equal(
           collapse_fct_levels(factor(NA_character_), new_levels = list("A-C" = "") ),
           factor(NA_character_, levels = "A-C")
         )
       })
  }
)

describe(
  "is_date() works", 
  {
    test_date <- as.Date("2023/01/01")
    it("gives a logical as output, which is TRUE if a date is provided 
       and FALSE otherwise", {
         expect_true(is_date(test_date))
         expect_false(is_date(as.character(test_date)))
         expect_false(is_date(NA))
       })
    it("errors if the input is not a vector", {
      expect_error(is_date(as.data.frame(test_date)))
    })
  }
)

describe(
  "cols_to_char() works", 
  {
    it("converts non-numeric columns (for example factors) to character", {
      expect_true(is.character(cols_to_char(iris)$Species))
    })
    it("errors with non-data.frame input", {
      expect_error(cols_to_char(list(iris, mtcars)))
      expect_error(cols_to_char(iris$Species))
    })
    
  }
)


describe(
  "date_cols_to_char() works", 
  {
    df <- mtcars
    df$dates <- Sys.Date() + sample(1:100, size = nrow(mtcars))
    it("converts date columns to character", {
      expect_true(is.character(date_cols_to_char(df)$dates))
    })
    it("errors with non-data.frame input", {
      expect_error(date_cols_to_char(list(iris, mtcars)))
      expect_error(date_cols_to_char(df$dates))
    })
    
  }
)

describe(
  "clean_dates() works", 
  {
    df <- mtcars
    df$dates <- Sys.Date() + sample(1:100, size = nrow(mtcars))
    it("cleans dates as expected and returns a date vector, 
       replacing unknown years with  date columns to character", {
         expect_equal(
           clean_dates(c("2023-08-NK", "2023-NK-NK", "NK-08-01")),
           as.Date(c("2023-08-01", "2023-01-01", NA_character_))
         )
       })
    it("returns dates unaltered", {
      expect_equal(
        clean_dates(as.Date(c("2023-08-01", "2023-01-01", NA_character_))),
        as.Date(c("2023-08-01", "2023-01-01", NA_character_))
      )
    })
    it("errors with incorrect input", {
      expect_error(clean_dates(mtcars))
      expect_error(clean_dates(list(mtcars)))
    })
  }
)


describe(
  "vector_select() Can include or exclude values from a vector based on 
  given regular expressions.", 
  {
    it("01 | Can include and exclude values based on a given regular expression", {
      expect_equal(
        vector_select(names(iris), include = "Sepal", exclude = "Width"),
        "Sepal.Length"
      )
    })
    it("02 | Can use multiple include statements to check for inclusion", {
      expect_equal(
        vector_select(names(iris), include = c("Sepal", "Species")),
        c("Sepal.Length", "Sepal.Width", "Species")
      )
    })
    it("03 | Can use multiple exclude statements to check for inclusion", {
      expect_equal(
        vector_select(names(iris), exclude = c("Width", "Species")),
        c("Sepal.Length", "Petal.Length")
      )
    })
    it("04 | Can use multiple include and exclude statements simultaneously", {
      expect_equal(
        vector_select(names(iris), include = c("Sepal", "Species"), 
                      exclude = c("Length", "Width")),
        "Species"
      )
    })
    it("05 | Handles NULL inputs and errors with non-vector inputs", {
      expect_equal(vector_select(NULL), character(0))
      expect_error(vector_select(iris))
      expect_error(vector_select(names(iris), include = data.frame()))
      expect_error(vector_select(names(iris), exclude = data.frame()))
      expect_equal(
        vector_select(names(iris), include = NULL, exclude = NULL),
        names(iris)
      )
    })
  }
)

describe("format_test_results() works", {
  it("errors with incorrect input", {
    expect_error(
      format_test_results(data.frame()),
      "Expecting an object of class 'testthat_results'"
    )
  })
  it("returns a list with expected output and informs that all tests passed", {
    res <- list(
      list(
        file = c("test-file1.R"),
        context = "",
        test = c("Test description"),
        user = 0.1234,
        system = 0,
        real = 0.145,
        results = list(
          expectation("success", "success message"), 
          expectation("success", "second success message"), 
          expectation("success", "third success message")
        )
      )
    )
    class(res) <- "testthat_results"
    # this approach instead of snapshot prevents the message being printed 
    # when running all tests, which might be confusing:
    temp.sink <- withr::local_tempfile(fileext = ".txt")
    sink(file = temp.sink)
    output <- format_test_results(res)
    sink()
    expect_equal(
      readLines(temp.sink),
      c(
        " failed skipped   error warning  passed ",
        "      0       0       0       0       3 ",
        "All tests passed successfully"
      )
    )
    expect_true(inherits(output, "list"))
    expect_true(inherits(output$results, "testthat_results"))
    expect_equal(
      names(output), 
      c("results", "time", "session", "sum_results", "test_outcome")
    )
  })
  
  
  it("returns a list with expected output, and prints a warning if there are failures", {
    res <- list(
      list(
        file = c("test-file1.R"),
        context = "",
        test = c("Test description"),
        user = 0.1234,
        system = 0,
        real = 0.145,
        results = list(
          expectation("success", "success message"), 
          expectation("failure", "first failure message"), 
          expectation("failure", "second failure message")
        )
      )
    )
    class(res) <- "testthat_results"
    temp.sink <- withr::local_tempfile(fileext = ".txt")
    sink(file = temp.sink)
    output <- suppressWarnings(format_test_results(res))
    sink()
    expect_equal(
      readLines(temp.sink),
      c(
        " failed skipped   error warning  passed ",
        "      2       0       0       0       1 ",
        "There was a failure in the following tests: ",
        "test-file1.R",
        "Failure messages: ",
        "",
        "first failure message",
        "",
        "second failure message"
      )
    )
    expect_true(inherits(output, "list"))
    expect_true(inherits(output$results, "testthat_results"))
    expect_equal(
      names(output), 
      c("results", "time", "session", "sum_results", "test_outcome")
    )
  })
})

describe("all_tests_passed() provides expected output", {
  it("returns TRUE when all pass", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 0, "error" = 0, "warning" = 0, "passed" = 10)
    )
    expect_true(all_tests_passed(res))
  })
  it("returns TRUE with a skipped test and [include_skipped] is FALSE", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 1, "error" = 0, "warning" = 0, "passed" = 10)
    )
    expect_true(all_tests_passed(res, include_skipped = FALSE))
  })
  it("returns FALSE with a failure", {
    res <- list(
      "sum_results" = c("failed" = 1, "skipped" = 0, "error" = 0, "warning" = 0, "passed" = 10)
    )
    expect_false(all_tests_passed(res))
  })
  it("returns FALSE with a skipped test and [include_skipped] is TRUE", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 1, "error" = 0, "warning" = 0, "passed" = 10)
    )
    expect_false(all_tests_passed(res))
  })
  it("returns FALSE with an error", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 0, "error" = 1, "warning" = 0, "passed" = 10)
    )
    expect_false(all_tests_passed(res))
  })
  it("returns FALSE with a warning", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 0, "error" = 0, "warning" = 1, "passed" = 10)
    )
    expect_false(all_tests_passed(res))
  })
  it("returns FALSE with no tests available", {
    res <- list(
      "sum_results" = c("failed" = 0, "skipped" = 0, "error" = 0, "warning" = 0, "passed" = 0)
    )
    expect_false(all_tests_passed(res))
  })
  it("returns NA with a warning if [var] does not exist in the results list", {
    res <- list("sum_results" = c())
    expect_warning(all_tests_passed(res, var = "non_existing_var"))
    expect_equal(
      suppressWarnings(all_tests_passed(res, var = "non_existing_var")), 
      NA
    )
  })
})

describe("expectation_type() works", {
  it("outputs TRUE if expectation type matches the provided type and FALSE when not", {
    types <- c("success", "failure", "warning", "error")
    lapply(types, \(x){
      expect_true(expectation_type(expectation(x, "foo"), x))
    })
    expect_false(expectation_type(expectation("success", "s"), "failure"))
    expect_false(expectation_type(expectation("failure", "f"), "success"))
  })
  it("errors with incorrect input", {
    expect_error(expectation_type(data.frame), "is.expectation")
    expect_error(expectation_type(expectation("failure", "f"), "non-existing expectation"))
  })
})

describe("decode_base64() works", {
  it("converts a base64-encoded string with special characters as expected", {
    testthat::skip_if_not_installed("base64enc")
    #base64enc::base64encode(charToRaw("Ťěšťůšěř 的"))
    expect_equal(
      decode_base64("xaTEm8WhxaXFr8WhxJvFmSDnmoQ="), 
      "Ťěšťůšěř 的"
      )
  })
  it("returns NULL if the input is NULL", {
    testthat::skip_if_not_installed("base64enc")
    expect_null(decode_base64(NULL))
  })
  it("warns if decoded output is not a UTF-8 encoded string and 
     returns a safe, quoted string in such a case", {
    testthat::skip_if_not_installed("base64enc")
    expect_warning(
      output <- decode_base64("I am not encoded"),
      paste0("decoded string does not contain valid UTF-8. ",
             "Is the input really base64 encoded?")
    )
    expect_equal(output, "\"!\\xa9\\xa7\\xa2קr\\x87^\"")
  })
})
