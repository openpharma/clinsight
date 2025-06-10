#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#'
#' @export
#'
app_ui <- function(request){
  add_study_logo <- !is.null(golem::get_golem_options("study_logo_path"))
  tagList(
    golem_add_external_resources(
      study_name = golem::get_golem_options("meta")$settings$study_name,
      add_logo = add_study_logo
    ),
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
        tags$img(src='www/clinsightlogo-app.png', height = '40')
      ), 
      sidebar = bslib::sidebar(mod_main_sidebar_ui("main_sidebar_1"), fillable = TRUE),
      header =   conditionalPanel(
        condition = "!['Start', 'Queries', 'Create Report'].includes(input.main_tabs) && !output.form_level_review",
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
        if(add_study_logo){
          tags$img(id = "study_logo", src = golem::get_golem_options("study_logo_path"), height = '40')
        } else{
          tags$h3(textOutput("study_name"), class = "text-secondary")
        }
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
golem_add_external_resources <- function(
    study_name = NULL, 
    add_logo = NULL
) {
  
  # If a study asset path is provided, verify it exists before adding it as a 
  # resource path
  logo_path <- get_golem_config("study_logo")
  if(isTRUE(add_logo) && !is.null(logo_path)){
    logo_name <- basename(logo_path)
    temp_logo_dir <- file.path(tempdir(), "clinsight_assets")
    dir.create(temp_logo_dir, showWarnings = FALSE)
    file.copy(logo_path, file.path(temp_logo_dir, logo_name), overwrite = TRUE)
    add_resource_path("assets", temp_logo_dir)
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
      app_title = if(is.null(study_name)){
        "ClinSight"
      } else {
        paste("ClinSight |", study_name)
      }
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
  
}
