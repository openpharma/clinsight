describe(
  "check_appdata works", 
  {
    it("tests appdata as expected", {
      appdata <- get_appdata(clinsightful_data)
        
      expect_warning(
        check_appdata(appdata, metadata), 
        regexp = "Not all variables defined in metadata are present"
        )
    })
  }
)
