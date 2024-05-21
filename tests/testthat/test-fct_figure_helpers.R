
describe(
  "remove_boxplot_outliers() removes boxplot outliers from plotly::ggplotly() 
  boxplots. ", 
  {
    fig <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + 
      ggplot2::geom_boxplot(outlier.shape = NA) + ggplot2::geom_point()
    plotly_fig <- plotly::ggplotly(fig)
    it("creates a valid plotly object", {
      expect_true(inherits(remove_boxplot_outliers(plotly_fig), "plotly"))
    })
    it("has all markers in boxplot layers set to zero", {
      expected_opacity <- lapply(remove_boxplot_outliers(plotly_fig)$x$data, 
                                 \(x){x$type != "box" || x$marker$opacity == 0})
      expect_true(all(unlist(expected_opacity)))
    })
  }
)

describe(
  "custom_plot_theme() applies a customized theme on a ggplot2 object", 
  {
    fig <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) +
      ggplot2::geom_point()
    
    it("renders without error and returns a ggplot2 object", {
      expect_true(ggplot2::is.ggplot(fig + custom_plot_theme()))
    })
  }
)

describe(
  "get_ggplot_layer_names() works", 
  {
    it("returns ggplot layers in a vector format", {
      fig <- ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg, group = cyl)) + 
        ggplot2::geom_boxplot() + 
        ggplot2::geom_point()
      expect_equal(get_ggplot_layer_names(fig), c("geom_boxplot", "geom_point"))
    })
    it("errors if the input is not a ggplot object", {
      expect_error(get_ggplot_layer_names("vector input"))
    })
  }
)
