#' optimizer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optimizer_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyWidgets::currencyInput(
      inputId = ns("budgetInput"),
      label = "Enter spending budget (in USD):",
      value = "$1000.00",
      format = "dollar",
      width = NULL,
      align = "center"
    ),
    shinyWidgets::prettyToggle(
      inputId = ns("integerToggle"),
      label_on = "Allow fractional shares", 
      icon_on = icon("check"),
      status_on = "success",
      status_off = "danger", 
      label_off = "Don't allow fractional shares",
      icon_off = icon("remove")
    ),
    shiny::selectizeInput(
      inputId = ns("assetPicker"),
      label = "Select between 2 and 10 asset tickers to optimize with:", 
      multiple = TRUE,
      choices = NULL,
      options = list(maxItems = 2, placeholder = 'Search or scroll to select assets')
    ),
    shinyWidgets::sliderTextInput(
      inputId = ns("monthSlider"),
      label = "Choose number of past months to optimize with:", 
      choices = seq(50, 2, by = -5),
      grid = TRUE
    )
  )
}

#' optimizer Server Functions
#'
#' @noRd 
mod_optimizer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    df <- shiny::reactive({memoise_tickers()})
    
    observeEvent(df(), {
      req(df())
      shiny::updateSelectizeInput(
        session, 
        inputId = "assetPicker",
        choice = split(df()$ticker, df()$assetType),
        server = TRUE)
    })
    
    observeEvent(input$assetPicker, {
      monthChoices <- df() |>
        dplyr::filter(.data$ticker %in% input$assetPicker) |>
        dplyr::select(.data$startDate) |>
        dplyr::pull() |>
        max(na.rm = T) |>
        lubridate::interval(Sys.Date()) %/% months(1) |>
        c(60) |>
        min(na.rm = T) |>
        seq(to = 2, by = -5)
      
      shinyWidgets::updateSliderTextInput(
        session,
        inputId = "monthSlider",
        choices = monthChoices
      )
    })
    
    
    
  })
}

## To be copied in the UI
# mod_optimizer_ui("optimizer_ui_1")

## To be copied in the server
# mod_optimizer_server("optimizer_ui_1")
