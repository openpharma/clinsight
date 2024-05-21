describe(
  "fig_boxplots(). Feature 1 | As a user, I should be able to create custom 
  boxplots.", 
  {
    set.seed(2023)
    fig_data <- data.frame(
      "site_code" = sample(c("Site 1", "Site 2"), 50, replace = TRUE),
      "item_value" = runif(50, 1, 100),
    #  "subject_id"  = sample(1:10, 50, replace = TRUE),
      "text_label" = "test label",
      "significance" = "test"
    )
    
    it("produces a ggplot object", {
      expect_true(ggplot2::is.ggplot(fig_boxplots(fig_data, sumval = 20)))
      expect_true(ggplot2::is.ggplot(fig_boxplots(
        fig_data, 
        sumval = 20,
        title = "main_title",
        y_title = "y-axis_title", 
        x_title = "x-axis_title"
        )))
    })
    it("has the right custom titles", {
      fig <- fig_boxplots(fig_data, 
                   title = "main_title",
                   y_title = "y-axis_title", 
                   x_title = "x-axis_title")
      expect_true(ggplot2::is.ggplot(fig)) 
      expect_equal(fig$labels$x, "x-axis_title")
      expect_equal(fig$labels$y, "y-axis_title")
      expect_equal(fig$labels$title, "main_title")
      
    })
  }
)
