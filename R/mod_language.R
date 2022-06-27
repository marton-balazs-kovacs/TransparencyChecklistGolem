#' language UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_language_ui <- function(id){
  ns <- NS(id)
  # Read language list
  language_list <- jsonlite::read_json(app_sys('app/www/language_list.json'))
  
  # Add select input
  tagList(
      selectInput(ns("language"), "Select Language", language_list, width = "auto") |> 
      with_i18n("Select Language", selector = "label")
  )
}
    
#' language Server Functions
#'
#' @noRd 
mod_language_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Change language
    observeEvent(input$language, {
      change_language(input$language)
      # Initiate language change on the whole app
      localize("html")
    }, ignoreInit = TRUE)
  })
}
    
## To be copied in the UI
# mod_language_ui("language")
    
## To be copied in the server
# mod_language_server("language")
