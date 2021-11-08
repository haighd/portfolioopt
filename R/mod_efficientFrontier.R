#' efficientFrontier UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList

mod_efficientFrontier_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      shiny::actionButton(
        ns("btnOptimize"),
        label = "Optimize Portfolio"
      ),
      # plotOutput(ns("efPlot")),
      title = "Efficient Frontier of Portfolio", 
      status = 'primary', 
      solidHeader = TRUE,
      width = 12
    )
  )
}

#' efficientFrontier Server Functions
#'
#' @noRd 
mod_efficientFrontier_server <- function(id, returns){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$btnOptimize, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Thank you for clicking')
    })
    
    # observeEvent(input$btnOptimize, {
    #   
    #   efFront <- efficientFrontier.fn(R = returns(),  nPorts = 60)
    #   plot_df <- 
    #     data.frame(
    #       x = efFront$EfficientFrontier$Risk*sqrt(252),
    #       y = efFront$EfficientFrontier$Return*252
    #     )
    #   
    # })
    
    
    # output$efPlot <- renderPlot({
    #   ggplot2::ggplot(
    #     data = plot_df,
    #     aes(x = x, y = y)
    #   ) +
    #     ggplot2::geom_line()
    #   
    # }) |>
    #   bindEvent(efVals$plot_df, ignoreInit = TRUE)
    
    
  })
}

## To be copied in the UI
# mod_efficientFrontier_ui("efficientFrontier_ui_1")

## To be copied in the server
# mod_efficientFrontier_server("efficientFrontier_ui_1")
