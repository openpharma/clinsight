describe(
  "fig_barplot works", 
  {
    set.seed(2023)
    df <- mtcars |> 
      dplyr::mutate(
        fill_color = sample(c(TRUE, FALSE), 1),
        .by = cyl
      )
    it("produces a ggplot", {
      expect_true(ggplot2::is.ggplot(fig_barplot(df, cyl, fill_color)))
    })
    it("plot object contains the expected data", {
      fig <- fig_barplot(df, cyl, fill_color)
      expect_equal(fig$data, df)
    })
    
    it("can change the axis titles", {
      fig <- fig_barplot(df, cyl, fill_color, x_lab = "X title", y_lab = "Y title")
      expect_true(ggplot2::is.ggplot(fig))
      expect_equal(fig$labels$x, "X title")
      expect_equal(fig$labels$y, "Y title")
    })
    
  }
)