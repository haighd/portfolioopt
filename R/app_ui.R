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
    # htmltools::htmlDependency(
    #   name = "font-awesome", version = "6.0.0",
    #   src = "./inst/app/www/font-awesome",
    #   stylesheet = "css/all.min.css"
    # ),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "Portfolio Optimizer & Backtester",
        titleWidth = 300
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        width = 300,
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem(
            "Optimizer", 
            tabName = "optimizer", 
            icon = icon("chart-line")
          ),
          shiny::conditionalPanel(
            "input.tabs == 'optimizer'",
            mod_optimizer_ui("optimizer_ui_1")
          )
        )
      ),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "optimizer",
            shinydashboardPlus::box(status = 'primary', solidHeader = TRUE)
          )
        )
      ),
      title = "Portfolio Optimizer & Backtester"
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
  )
}

