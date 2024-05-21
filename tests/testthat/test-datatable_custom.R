describe("datatable_custom() works", {
  it("creates a datatable", {
    expect_true(inherits(datatable_custom(mtcars), "datatables"))
  })
  it("creates a datatable and can change column names", {
    outcome <- datatable_custom(mtcars, c("mpg_new" = "mpg", "cyl_new" = "cyl"))
    expect_true(inherits(datatable_custom(mtcars), "datatables"))
    expect_equal(
      names(outcome$x$data), 
      c(" ", "mpg_new", "cyl_new", names(mtcars)[-c(1,2)]) 
      )
  })
  it("creates a datatable and can add a title to the table", {
    outcome <- datatable_custom(iris, title = "Test title")
    expect_true(inherits(outcome, "datatables"))
    expect_true(grepl("Test title", outcome$x$caption))
  })
  it("Produces a valid datatable with zero rows if no rows provided in the input table", {
    outcome <- datatable_custom(iris[0,])
    expect_true(inherits(outcome, "datatables"))
    # -1 to remove the standard row name column: 
    expect_equal(outcome$x$data[, -1], iris[0, ])
  })
  it("Errors if not a valid data frame is provided", {
    expect_error(datatable_custom(NULL))
  })
  it("Errors if rename_vars is not a character vector", {
    expect_error(datatable_custom(iris, rename_vars = data.frame()))
      })
  it("Errors if title is not a character vector", {
    expect_error(datatable_custom(iris, title = data.frame()))
  })
})
