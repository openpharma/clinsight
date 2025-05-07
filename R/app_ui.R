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
    bundle_resources_custom(
      path = app_sys("app/www"),
      app_title = "clinsight"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


#' Automatically serve golem external resources
#'
#' This function is a wrapper around `htmltools::htmlDependency` that
#' automatically bundles the CSS and JavaScript files in `inst/app/www`
#' and which are created by `golem::add_css_file()` , `golem::add_js_file()`
#' and `golem::add_js_handler()`.
#'
#' This function also preload [activate_js()] which allows to
#' use preconfigured JavaScript functions via [invoke_js()].
#'
#' @param path The path to the folder where the external files are located.
#' @param app_title The title of the app, to be used as an application title.
#' @param app_builder The name of the app builder to add as a meta tag.
#' Turn to NULL if you don't want this meta tag to be included.
#' @inheritParams htmltools::htmlDependency
#' @param with_sparkles C'est quand que tu vas mettre des paillettes dans ma vie Kevin?
#'
#' @importFrom htmltools htmlDependency
#'
#' @return an htmlDependency
bundle_resources_custom <- function(
    path,
    app_title,
    name = "golem_resources",
    version = "0.0.1",
    meta = NULL,
    head = NULL,
    attachment = NULL,
    package = NULL,
    all_files = TRUE,
    app_builder = "golem",
    with_sparkles = FALSE
) {
  res <- list()
  if (
    length(
      list.files(path)
    ) > 0
  ) {
    res[[
      length(res) + 1
    ]] <- htmltools::htmlDependency(
      name,
      version,
      src = path,
      script = list.files(
        path,
        pattern = "\\.js$",
        recursive = TRUE
      ),
      meta = c("app-builder" = app_builder, meta),
      head = c(
        as.character(
          tags$title(app_title)
        ),
        # as.character(
        #   golem::activate_js()
        # ),
        head
      ),
      attachment = attachment,
      package = package,
      all_files = all_files
    )
    # For some reason `htmlDependency` doesn't bundle css,
    # So add them by hand
    css_list <- list.files(
      path,
      pattern = "\\.css$",
      recursive = TRUE
    )
    
    if (length(css_list) > 0) {
      css_nms <- paste0(
        basename(path),
        "/",
        list.files(
          path,
          pattern = "\\.css$",
          recursive = TRUE
        )
      )
      
      for (i in css_nms) {
        res[[
          length(res) + 1
        ]] <- tags$link(
          href = i,
          rel = "stylesheet"
        )
      }
    }
  }
  
  if (with_sparkles) {
    res[[
      length(res) + 1
    ]] <- htmlDependency(
      "sparkles",
      version = utils::packageVersion("golem"),
      src = system.file(
        "utils",
        package = "golem"
      ),
      script = "sparkle.js"
    )
  }
  
  res
}