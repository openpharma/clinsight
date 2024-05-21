describe(
  "fig_timeseries works", 
  {
    library(ggplot2)
    subjects <- paste0("Subject", 1:10)
    mock_data <- lapply(subjects, \(x){
      data.frame(
        subject_id = x,
        day = sample(1:25, 10),
        item_name = sample(c("item1", "item2"), 10, replace = TRUE),
        item_value = runif(10, 0 , 50),
        significance = sample(names(col_palette), 10, replace = TRUE),
        text_label = "test text",
        reviewed = sample(c("Yes", "No"), replace = TRUE)
      )}
    ) |>
      dplyr::bind_rows() |> 
      dplyr::mutate(
        upper_lim = max(item_value),
        lower_lim = min(item_value),
        value_scaled = (item_value-lower_lim)/(upper_lim-lower_lim),
        .by = item_name
      )
    
    it("outputs a ggplot2 object with a line plot", {
      expect_true(is.ggplot(fig_timeseries(mock_data, id_to_highlight = "Subject10")))
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject10")
      
    })
    it("uses scaled limits and adds limits at y=0  and y=1 if requested", {
      expect_true(
        is.ggplot(fig_timeseries(mock_data, id_to_highlight = "Subject10", scale = TRUE))
      )
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject10", scale = TRUE)
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(length(plotlayers[plotlayers == "geom_hline"]), 2)
      expect_equal(fig$data, mock_data)
    })
    
    it("can add two horizontal lines with data-defined limits to the a ggplot2 object", {
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject9", use_unscaled_limits = TRUE)
      expect_true(is.ggplot(fig))
      expect_equal(fig$data, mock_data)
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(length(plotlayers[plotlayers == "geom_hline"]), 2)
    })
    
    it("returns a plot without highlight if the id to hightlight has no data for the figure.", {
      fig_timeseries(mock_data, id_to_highlight = "Subject15")
    })
    
  }
)
