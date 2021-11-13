#' chartAssets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rlang .data

mod_chartAssets_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        id = ns("tabset1"),
        width = 12,
        shiny::tabPanel(
          title = "Cumulative Returns", 
          plotOutput(ns("returnPlot"))
        ),
        shiny::tabPanel(
          title = "Return Distributions", 
          plotOutput(ns("boxPlot"))
        ),
        shiny::tabPanel(
          title = "Risk vs. Reward",
          plotOutput(ns("riskRewardPlot"))
        )
      )
    )
  )
}

#' chartAssets Server Functions
#'
#' @noRd 
mod_chartAssets_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    assetVals <- reactiveValues()
    
    toListen <- reactive({
      list(vals()$assets, vals()$start_date)
    })
    
    observeEvent(toListen(), {
      req(nrow(vals()$assets) > 0)
      df <- vals()$assets
      for(row in seq(1:nrow(df))){
        prices(df[row, 'ticker'] |> dplyr::pull(), df[row, 'assetType'], vals()$start_date)
        if(exists("res")){
          res <- xts::merge.xts(res, prices(df[row, 'ticker'] |> dplyr::pull(), df[row, 'assetType'] |> dplyr::pull(), vals()$start_date))
        } else {
          res <- prices(df[row, 'ticker'] |> dplyr::pull(), df[row, 'assetType'], vals()$start_date)
        }
      }
      
      returns <- PerformanceAnalytics::CalculateReturns(res)
      assetVals$returns <- stats::na.omit(returns)
      
    }, ignoreNULL = TRUE)
    
    output$returnPlot <- renderPlot({
      req(assetVals$returns)
      PerformanceAnalytics::chart.CumReturns(
        assetVals$returns, 
        legend = "topleft", 
        colorset=PerformanceAnalytics::rich6equal, 
        lwd=2, 
        ylog=TRUE)
    })
    
    output$boxPlot <- renderPlot({
      req(assetVals$returns)
      my_boxplot(
        assetVals$returns
      )
    })
    
    # output$months <- renderPrint({
    #   HTML(
    #     cat(
    #       "Trailing </font><font color='orange'>", 
    #       vals()$months, 
    #       "</font>-Month Returns"), "</font>", sep = "")
    # })
    
    output$riskRewardPlot <- renderPlot({
      req(assetVals$returns)
      PerformanceAnalytics::chart.RiskReturnScatter(
        assetVals$returns, 
        Rf=.03/12, 
        colorset=PerformanceAnalytics::rich6equal
        )
    })
    
    return(assetVals)
    
  })
}

## To be copied in the UI
# mod_chartAssets_ui("chartAssets_ui_1")

## To be copied in the server
# mod_chartAssets_server("chartAssets_ui_1")
