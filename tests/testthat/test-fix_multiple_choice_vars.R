describe(
  "fix_multiple_choice_vars() works", 
  {
    df <- data.frame(
      ID = "Subj1",
      var = c("Age", paste0("MH_TRT", 1:4)),
      item_value = as.character(c(95, 67, 58, 83, 34))
    )
    
    it("gives the expected output", {
      expect_true(is.data.frame(fix_multiple_choice_vars(df, common_vars = "ID")))
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT"),
        item_value = c("95", "67; 58; 83; 34")
      )
      expect_equal(fix_multiple_choice_vars(df, common_vars = "ID"), expected)
    })
    it("returns the same df if no mc vars are found", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT"),
        item_value = as.character(c(95, 67))
      )
      expect_equal(fix_multiple_choice_vars(df, common_vars = "ID"), df)
    })
    it("returns the same data frame if no missing vars are found", {
      df <- data.frame(
        var = metadata$items_expanded$var,
        ID = rep("Subj1", times = length(metadata$items_expanded$var)),
        item_value = c("")
      )
      expect_equal(fix_multiple_choice_vars(df, common_vars = "ID"), df)
    })
    
  }
)