describe(
  "fig_timeseries works", 
  {
    set.seed(2025)
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
      expect_true(ggplot2::is_ggplot(fig_timeseries(mock_data, id_to_highlight = "Subject10")))
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject10")
      
    })
    it("uses scaled limits and adds limits at y=0  and y=1 if requested", {
      expect_true(
        ggplot2::is_ggplot(fig_timeseries(mock_data, id_to_highlight = "Subject10", yval = "value_scaled"))
      )
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject10", yval = "value_scaled")
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(length(plotlayers[plotlayers == "geom_hline"]), 2)
      expect_equal(fig$data, mock_data)
    })
    
    it("can add two horizontal lines with data-defined limits to the a ggplot2 object", {
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject9", use_unscaled_limits = TRUE)
      expect_true(ggplot2::is_ggplot(fig))
      expect_equal(fig$data, mock_data)
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(length(plotlayers[plotlayers == "geom_hline"]), 2)
    })
    
    it("returns a spaghetti plot without highlight if the id to hightlight has no data for the figure.", {
      fig <- fig_timeseries(mock_data, id_to_highlight = "Subject15")
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(plotlayers, "geom_line")
      expect_equal(mock_data, fig$data)
    })
    
    it("returns a spaghetti plot without highlight if the id to hightlight is NA", {
      fig <- fig_timeseries(mock_data, id_to_highlight = NA)
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(plotlayers, "geom_line")
      expect_equal(mock_data, fig$data)
    })
    
    it("returns a spaghetti plot without highlight if the id to hightlight is NULL", {
      fig <- fig_timeseries(mock_data, id_to_highlight = NULL)
      plotlayers <- get_ggplot_layer_names(fig)
      expect_equal(plotlayers, "geom_line")
      expect_equal(mock_data, fig$data)
    })
    
    it("includes time points that are negative days (days before baseline)", {
      # set minimum day to -10:
      negative_day_data <- mock_data |> 
        dplyr::mutate(
          day = ifelse(day == min(day), - 10, day),
          .by = c(subject_id, item_name)
        )
      fig <- fig_timeseries(negative_day_data, id_to_highlight = "Subject1")
      fig_built <- ggplot2::ggplot_build(fig)
      expect_equal(
        min(fig_built[["layout"]]$panel_scales_x[[1]]$range$range),
        -10
      )
    })
    
  }
)
