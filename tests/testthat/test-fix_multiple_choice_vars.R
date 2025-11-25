describe(
  "fix_multiple_choice_vars() works", 
  {
    df <- data.frame(
      ID = "Subj1",
      var = c("Age", paste0("MH_TRT", 1:4)),
      item_value = as.character(c(95, 67, 58, 83, 34))
    )
    
    it("gives the expected output", {
      expect_true(is.data.frame(fix_multiple_choice_vars(df, key_cols = "ID")))
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT"),
        item_value = c("95", "67; 58; 83; 34")
      )
      expect_equal(fix_multiple_choice_vars(df, key_cols = "ID"), expected)
    })
    it("returns the same df if no mc vars are found", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT"),
        item_value = as.character(c(95, 67))
      )
      expect_equal(fix_multiple_choice_vars(df, key_cols = "ID"), df)
    })
    it("returns the same data frame if no missing vars are found", {
      df <- data.frame(
        var = metadata$items_expanded$var,
        ID = rep("Subj1", times = length(metadata$items_expanded$var)),
        item_value = c("")
      )
      expect_equal(fix_multiple_choice_vars(df, key_cols = "ID"), df)
    })
    it("also works if expected vars end with a number", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", paste0("MH_TRT1", 1:4)),
        item_value = as.character(c(95, 67, 58, 83, 34))
      )
      expect_no_error({
        outcome <- fix_multiple_choice_vars(
          df, 
          expected_vars = c("Age", "MH_TRT1"), 
          key_cols = "ID"
        )
      })
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1"),
        item_value = c("95", "67; 58; 83; 34")
      )
      expect_equal(outcome, expected)
    })
    it("removes one missing value in the collapsed string of a multiple choice variable", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", paste0("MH_TRT1", 1:4)),
        item_value = as.character(c(95, 67, NA, 83, 34))
      )
      outcome <- fix_multiple_choice_vars(
        df, 
        expected_vars = c("Age", "MH_TRT1"), 
        key_cols = "ID"
      )
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1"),
        item_value = c("95", "67; 83; 34")
      )
      expect_equal(outcome, expected)
      
    })
    it("removes multiple missing values in the collapsed string of a multiple choice variable", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", paste0("MH_TRT1", 1:4)),
        item_value = as.character(c(95, NA, NA, NA, 34))
      )
      outcome <- fix_multiple_choice_vars(
        df, 
        expected_vars = c("Age", "MH_TRT1"), 
        key_cols = "ID"
      )
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1"),
        item_value = c("95", "34")
      )
      expect_equal(outcome, expected)
    })
    it("returns a missing value if all values of a multiple choice variable are missing", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", paste0("MH_TRT1", 1:4)),
        item_value = as.character(c(95, NA, NA, NA, NA))
      )
      outcome <- fix_multiple_choice_vars(
        df, 
        expected_vars = c("Age", "MH_TRT1"), 
        key_cols = "ID"
      )
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1"),
        item_value = c("95", NA)
      )
      expect_equal(outcome, expected)
    })
    it("maintains the original order within a data frame after fixing multiple choice variables", {
      df <- data.frame(
        ID = "Subj1",
        var = c("Age", paste0("MH_TRT1", 1:4), "Sex", paste0("preferred_flavors", 1:4)),
        item_value = as.character(c(95, 12, 18, 76, 34, "Male", "banana", "chocolate", NA, "pistache"))
      )
      outcome <- fix_multiple_choice_vars(
        df, 
        expected_vars = c("Age", "MH_TRT1", "Sex", "preferred_flavors"), 
        key_cols = "ID"
      )
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1", "Sex", "preferred_flavors"),
        item_value = c("95", "12; 18; 76; 34", "Male", "banana; chocolate; pistache")
      )
      expect_equal(outcome, expected)
    })
    
    it("keeps the fixed multiple choice variable at the place of first appearance 
       within a data frame if the MC answers are not grouped together in the data frame", {
      df <- data.frame(
        ID = "Subj1",
        ### MH_TRT1 is now not grouped together anymore:
        var = c("Age", "MH_TRT11", "MH_TRT12", "Sex", "MH_TRT13", "MH_TRT14", paste0("preferred_flavors", 1:4)),
        item_value = c(95, 12, 18, NA, 76, 34, "banana", "chocolate", NA, "pistache")
      )
      outcome <- fix_multiple_choice_vars(
        df, 
        expected_vars = c("Age", "MH_TRT1", "Sex", "preferred_flavors"), 
        key_cols = "ID"
      )
      expected <- data.frame(
        ID = "Subj1",
        var = c("Age", "MH_TRT1", "Sex", "preferred_flavors"),
        item_value = c("95", "12; 18; 76; 34", NA, "banana; chocolate; pistache")
      )
      expect_equal(outcome, expected)
    })
  }
)