#' efficientFrontier UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom tidyselect vars_select_helpers
#' @importFrom rlang .data

mod_efficientFrontier_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        id = ns("tabset2"),
        width = 12,
        title = actionButton(ns("btnOptimize"), label = "Optimize"),
        shiny::tabPanel(
          title = "Risk vs. Returns", 
          plotOutput(ns("efPlot"))
        ),
        shiny::tabPanel(
          title = "Asset Weights", 
          plotOutput(ns("weightsPlot"))
        ),
        shiny::tabPanel(
          title = "Minimum Variance Optimal Portfolio",
          tableOutput(ns("mvp"))
        )
      )
    )
  )
}

#' efficientFrontier Server Functions
#'
#' @noRd 
mod_efficientFrontier_server <- function(id, assetVals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals <- reactiveValues(
      returns = NULL,
      efFront = NULL,
      plot_df = NULL
    )
    
    observeEvent(input$btnOptimize, {
      req(!is.null(assetVals()$returns))
      
      vals$returns <- assetVals()$returns
      
      vals$efFront <- efficientFrontier.fn(R = vals$returns,  nPorts = 60)
      
      vals$plot_df <- data.frame(
        x = vals$efFront$EfficientFrontier$Risk*sqrt(252),
        y = vals$efFront$EfficientFrontier$Return*252
      )
      
      weight_df <- vals$efFront$Weights |>
        data.frame() |>
        tibble::rowid_to_column("iteration")
      
      risk_df <- data.frame(risk = vals$efFront$EfficientFrontier$Risk*sqrt(252)) |>
        tibble::rowid_to_column("iteration")
      
      # print(risk_df)
      
      vals$plot_df_w <- weight_df |>
        tidyr::pivot_longer(cols = -c("iteration"), names_to = "name", values_to = "weight") |>
        dplyr::left_join(risk_df, by = "iteration")
      
      vals$mvp <- vals$efFront$mvp$weights
      
      vals$mvp <- vals$mvp |> 
        data.frame()
      
      vals$mvp <- vals$mvp |>
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::vars_select_helpers$where(is.numeric), 
            .fns = ~ paste0(100 * round(.x, digits = 3), "%")
          )
        )
      
      print(vals$mvp)
      
    })
    
    output$efPlot <- renderPlot({
      req(!is.null(vals$plot_df))
      ggplot2::ggplot(
        data = vals$plot_df,
        ggplot2::aes(x = .data$x, y = .data$y)
      ) +
        ggplot2::geom_line() +
        ggplot2::labs(x = 'Risk', y = 'Return')
    })
    
    output$weightsPlot <- renderPlot({
      req(!is.null(vals$plot_df_w))
      ggplot2::ggplot(
        data = vals$plot_df_w,
        ggplot2::aes(x = .data$risk, y = .data$weight, fill = .data$name)
      ) +
        ggplot2::geom_bar(stat = 'identity', width = 0.9)
    })
    
    output$mvp <- renderTable({vals$mvp}, rownames = TRUE, colnames = FALSE, striped = TRUE)
    
  })
}

## To be copied in the UI
# mod_efficientFrontier_ui("efficientFrontier_ui_1")

## To be copied in the server
# mod_efficientFrontier_server("efficientFrontier_ui_1")
