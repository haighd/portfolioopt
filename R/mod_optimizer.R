#' optimizer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom lubridate %m-%
#' 
mod_optimizer_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # shinyWidgets::currencyInput(
    #   inputId = ns("budgetInput"),
    #   label = "Enter spending budget (in USD):",
    #   value = "$1000.00",
    #   format = "dollar",
    #   width = NULL,
    #   align = "center"
    # ),
    # shinyWidgets::prettyToggle(
    #   inputId = ns("integerToggle"),
    #   label_on = "Allow fractional shares", 
    #   icon_on = icon("check"),
    #   status_on = "success",
    #   status_off = "danger", 
    #   label_off = "Don't allow fractional shares",
    #   icon_off = icon("remove")
    # ),
    shinyWidgets::sliderTextInput(
      inputId = ns("monthSlider"),
      label = "Choose number of past months to optimize with:", 
      selected = 60,
      choices = seq(2, 120),
      to_fixed = 2
    ),
    shiny::selectizeInput(
      inputId = ns("assetPicker"),
      label = "Select between 2 and 10 asset tickers to optimize with:", 
      multiple = TRUE,
      choices = NULL,
      options = list(maxItems = 10, placeholder = 'Search or scroll to select assets')
    )#,
    # shinyWidgets::materialSwitch(
    #   inputId = ns("weightSwitch"),
    #   label = "Include sum of weights constraint?", 
    #   value = FALSE,
    #   status = "primary",
    #   width = "100%"
    # ),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("weightSlider"),
    #   label = "Choose required sum of all asset weights",
    #   choices = seq(0, 1, 0.1),
    #   selected = 0.5,
    #   width = "100%"
    # ),
    # shinyWidgets::materialSwitch(
    #   inputId = ns("boxSwitch"),
    #   label = "Include min/max limits for all individual asset weights?", 
    #   value = FALSE,
    #   status = "primary",
    #   width = "100%"
    # ),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("boxSlider"),
    #   label = "Choose min/max limits for all individual assets",
    #   choices = seq(0, 1, 0.1),
    #   selected = c(0, 1),
    #   width = "100%"
    # ),
    # shiny::actionButton(
    #   inputId = ns("btnOptimize"),
    #   label = "Optimize Portfolio",
    #   icon = shiny::icon("glyphicon-equalizer", lib = "glyphicon")
    # )
  )
}

#' optimizer Server Functions
#'
#' @noRd 
mod_optimizer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals <- shiny::reactiveValues()
    
    observe({
      shinyjs::toggle("weightSlider", condition=input$weightSwitch)
    })
    
    observe({
      shinyjs::toggle("boxSlider", condition=input$boxSwitch)
    })
    
    df <- shiny::reactive({tickers()})
    
    observeEvent(df(), {
      req(df())
      shiny::updateSelectizeInput(
        session, 
        inputId = "assetPicker",
        choice = split(df()$ticker, df()$assetType),
        server = TRUE)
      
      print(unique(df()$assetType))
    })
    
    observeEvent(input$assetPicker, {
      vals$maxMonths <- df() |>
        dplyr::filter(.data$ticker %in% input$assetPicker) |>
        dplyr::select(.data$startDate) |>
        dplyr::pull() |>
        max(na.rm = T) |>
        lubridate::interval(Sys.Date()) %/% months(1) |>
        c(120) |>
        min(na.rm = T)# |>
      # seq(to = 2, by = -1)
      
      vals$valueUpdate <- min(vals$maxMonths, input$monthSlider)
      
      shinyWidgets::updateSliderTextInput(
        session,
        inputId = "monthSlider",
        selected = vals$valueUpdate,
        choices = seq(2, vals$maxMonths, 2),
      )
    })
    
    shiny::observe({
      vals$start_date <- Sys.Date() %m-% months(input$monthSlider)
      vals$months <- input$monthSlider
      vals$weightsToggle <- input$weightSwitch
      vals$weightsCons <- input$weightSlider
      vals$boxToggle <- input$boxSwitch
      vals$boxCons <- input$boxSlider
    })
    
    shiny::observe({
      req(df())
      vals$assets <- df() |>
        dplyr::filter(.data$ticker %in% input$assetPicker)
    })
    
    # shiny::observeEvent(input$btnOptimize, {
    #   vals$optimize <- input$btnOptimize
    # })
    
    # shiny::observe({
    #   vals$toggle <- input$integerToggle
    # })
    # 
    # shiny::observe({
    #   vals$budget <- input$budgetInput
    # })
    
    return(shiny::reactive({vals}))
    
  })
}

## To be copied in the UI
# mod_optimizer_ui("optimizer_ui_1")

## To be copied in the server
# mod_optimizer_server("optimizer_ui_1")
