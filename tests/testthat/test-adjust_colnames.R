
describe(
  "adjust_colnames() can adjust column names by replacing a pattern with a replacement string.", {
    it(
      "Outputs a valid data frame", 
      {
        expect_true(is.data.frame(adjust_colnames(iris, "^Sepal", "Flower")))
      })
    it("Errors with a missing input data frame object [data]", {
      expect_error(adjust_colnames(NA, "^Sepal", "Flower"))
    })
    it("Errors with a missing input [pattern]", {
      expect_error(adjust_colnames(iris, NA, "Flower"))
    })
    it("Errors with a missing [replacement] pattern", {
      expect_error(adjust_colnames(iris, "^Sepal", NA))
    })
    it("Adjusts names as intended, with the column name [pattern] being replaced 
       by the provided [replacement] string", {
         expect_equal(names(adjust_colnames(iris, "^Sepal", "Flower")), 
                      c("Flower.Length", "Flower.Width", "Petal.Length", 
                        "Petal.Width", "Species")) 
       })
  }
)
