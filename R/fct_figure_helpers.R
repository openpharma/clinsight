#' Remove boxplot outliers
#' 
#' Function to remove boxplot outliers and hover info from a [plotly::ggplotly()] 
#' object.
#' Only boxplot layers will be altered. Works also in faceted `ggplotly` figures. 
#' This is needed since boxplot outliers will cannot be removed in interactive 
#' `plotly` figures due to a bug. See also 
# [here](https://github.com/plotly/plotly.R/issues/1114#issuecomment-1239268701).
#'
#' @param fig A `plotly` object created with [plotly::ggplotly()]. 
#'
#' @return The same [plotly::ggplotly()] object, now with outliers in boxplots deleted.
#' @export 
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#' set.seed(2023) # for reproducible jitter
#' fig <- ggplotly({
#'   ggplot(iris, aes(factor(1), Petal.Width)) + geom_boxplot(outlier.shape = NA) + 
#'     geom_jitter(col = "red") + 
#'     facet_wrap(~Species)
#'  })
#'  # Outliers are visible in black in the plotly figure, even though they should not:
#' fig
#' 
#' #The function `remove_boxplot_outliers()` corrects for this:
#' remove_boxplot_outliers(ggplotly(fig))
#' 
remove_boxplot_outliers <- function(fig){
  stopifnot("plotly" %in% class(fig))
  fig$x$data <- lapply(
    fig$x$data,
    \(i){
      if(i$type != "box") return(i)
      i$marker = list(opacity = 0)
      i$hoverinfo = "none"
      i
    }
  )
  fig
}


#' Custom plot theme
#' 
#' A custom `ggplot2` theme to be used in the application.
#'
#' @return A list with ggplot2 theme layers
#' @export
#'
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, cyl)) + 
#'   geom_point() + 
#'   custom_plot_theme()
custom_plot_theme <- function(){
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      strip.background = ggplot2::element_rect(color = "transparent", fill = "transparent"),
      strip.text.x = ggplot2::element_text(size = 11),
      panel.grid.major.y = ggplot2::element_line(colour = "grey80"),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none"
    ),
    ggplot2::labs(
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    )
  )
}

#' Get ggplot layer names
#' 
#' Helper function to extract ggplot layer names from a ggplot object
#'
#' @param fig A ggplot2 object. 
#'
#' @return A character vector with all the layer names of the ggplot object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' fig <- ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) + geom_boxplot() + geom_point()
#' get_ggplot_layer_names(fig)
#' 
get_ggplot_layer_names <- function(fig){
  stopifnot(ggplot2::is.ggplot(fig))
  lapply(fig[["layers"]], \(x){
    lrs <- toString(x[["constructor"]][[1]]) |> 
      strsplit(", ") |> 
      unlist()
    lrs[grepl("^geom_",  lrs)]
  }) |> 
    unlist()
}
