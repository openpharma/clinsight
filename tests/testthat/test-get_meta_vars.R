describe(
  "get_meta_vars collects data and metadata in one list object", 
  {
    appdata <- get_appdata(clinsightful_data)
    
    it("creates expected output", {
      expect_snapshot(get_meta_vars(appdata, metadata))
    })
    it("errors with incorrect input", {
      expect_error(get_meta_vars(mtcars, metadata))
      expect_error(get_meta_vars(appdata, mtcars))
      expect_error(get_meta_vars(list(), mtcars), 
                   "Empty list with data provided")
    })
  }
)