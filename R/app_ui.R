#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinyApp(
      ui = dashboardPage(
        options = list(sidebarExpandOnHover = TRUE),
        header = dashboardHeader(),
        sidebar = dashboardSidebar(minified = TRUE, collapsed = TRUE),
        body = dashboardBody(
          lapply(1:20, box, width = 12, title = "box")
        ),
        controlbar = dashboardControlbar(),
        title = "DashboardPage"
      ),
      server = function(input, output) { }
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'portfolioopt'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

