#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#'
#' @export
#'
app_ui <- function(request){
  meta <- golem::get_golem_options("meta")
  tagList(
    golem_add_external_resources(study_name = meta$settings$study_name %||% "Study"),
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
        tags$img(src='www/logo_in_app_w_margin_wide.png', height = '40')
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
      bslib::nav_item(
        uiOutput("study_name_display")
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
golem_add_external_resources <- function(study_name) {
  
  # If a study asset path is provided, verify it exists before adding it as a 
  # resource path
  asset_path <- get_golem_config("study_asset_path")
  if(!is.null(asset_path)){
    if(dir.exists(asset_path)) {
      add_resource_path("assets", asset_path)
    } else {
      warning("The golem config called 'study_asset_path' is a non-existent directory. Please create the directory and re-launch the app.")
    }
  }
  # Add app/www to resource path as simply 'www/'
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = paste("ClinSight", study_name)
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
  
}
