#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  vals <- mod_optimizer_server("optimizer_ui_1")
  returns <- mod_chartAssets_server("chartAssets_ui_1", vals)
  mod_efficientFrontier_server("chartAssets_ui_1", returns)
}
