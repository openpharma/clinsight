test_that("get_base_value can add base values to a data frame ", {
  df <- as.data.frame(EuStockMarkets) |> 
    dplyr::mutate(time = time(EuStockMarkets)) |>
    tidyr::pivot_longer(-time)
  output <- get_base_value(df, c("name" = "DAX"), event = c("time" = df$time[1]),
                           value_column = "value", id_column = NULL)
  expected_base_value <- with(df, value[name == "DAX"])[1]
  expect_equal(unique(with(output, base_dax)), expected_base_value)
})

test_that("get_base_value handles zero-row data frames and errors with incorrect input values", {
  df <- as.data.frame(EuStockMarkets) |> 
    dplyr::mutate(time = time(EuStockMarkets)) |>
    tidyr::pivot_longer(-time)
  # returns an empty data frame when a data frame with zero rows was used as input
  output <- get_base_value(df[0,], c("name" = "DAX"), event = c("time" = df$time[1]),
                           value_column = "value", id_column = NULL)
  expected_outcome <- df[0, ] |> 
    dplyr::mutate(base_dax = double(0))
  
  expect_equal(output, expected_outcome)
  
  expect_error(
    get_base_value(df, c("DAX"), event = c("time" = df$time[1]),
                   value_column = "value", id_column = NULL),
    "'var' should be a named character vector"
  )
  expect_error(
    get_base_value(df, c("name" = "DAX"), event = "time",
                   value_column = "value", id_column = NULL),
    "'event' should be a named character vector"
  )
})

test_that("get_base_value handles missing base values and missing variable names", {

  df <- as.data.frame(EuStockMarkets) |> 
    dplyr::mutate(time = time(EuStockMarkets)) |>
    tidyr::pivot_longer(-time)
  output <- get_base_value(df, c("name" = "DAX"), event = c("time" = 00000),
                           value_column = "value", id_column = NULL)
  expect_equal(unique(output$base_dax), NA_integer_)
  
  output <- get_base_value(df, c("name" = "non-existent"), event = c("time" = df$time[1]),
                           value_column = "value", id_column = NULL)
  expect_equal(unique(output$base_non_existent), NA_integer_)
  
  # in a long data frame format, the value column is mandatory to exist and 
  # therefoer an error is warranted if it is not found:
  expect_error(
    get_base_value(df, c("name" = "DAX"), event = c("time" = df$time[1]),
                           value_column = "NON-EXISTENT", id_column = NULL)
  )
})
