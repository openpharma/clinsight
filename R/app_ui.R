#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#'
#' @export
#'
app_ui <- function(request){
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    bslib::page_navbar(
      id = "main_tabs",
      window_title = "Interactive Trial Safety Surveillance Tool",
      theme = bslib::bs_theme(
        bootswatch = "spacelab", 
        version = "5",  
        # this disables 'compact' value-boxes introduced in bslib 0.6.0
        # see https://github.com/rstudio/bslib/issues/963
        "bslib-value-box-horizontal-break-point" = "1px"
        ),
      bg = "#43464c",
      title = tags$a(
        href = "/",
        tags$img(src='www/logo_in_app_w_margin.png', height = '45')
      ), 
      sidebar = bslib::sidebar(mod_main_sidebar_ui("main_sidebar_1"), fillable = TRUE),
      header =   conditionalPanel(
        condition = "!['Start', 'Queries', 'Create Report'].includes(input.main_tabs)",
        mod_header_widgets_ui("header_widgets_1")
      ),
      bslib::nav_panel(
        title = "Start",
        mod_start_page_ui("start_page_1")
      ),
      bslib::nav_panel(
        title = "Common events",
        bslib::navset_tab(id = "common_data_tabs") 
      ),
      bslib::nav_panel(
        "Study data", 
        bslib::navset_tab(id = "study_data_tabs")
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        "Queries", 
        mod_queries_ui("queries_1")
      ),
      bslib::nav_item(
        mod_report_ui("report_1"),
        HTML(" |")
      ),
      bslib::nav_item(textOutput("user_info"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "clinsight"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
