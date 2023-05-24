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
  ## To use the local version of the language list uncomment the next line
  # language_list <- jsonlite::read_json(app_sys('app/www/language_list.json'))
  language_list <- jsonlite::read_json("https://raw.githubusercontent.com/marton-balazs-kovacs/TransparencyChecklistGolem/master/inst/app/www/language_list.json")
  
  # Add select input
  tagList(
      selectInput(NS(id, "language"), "Select Language", language_list, width = "auto") |> 
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
      # get_language(ns("current_lang"))
      change_language(input$language)
      # Initiate language change on the whole app
      localize("html")
    }, ignoreInit = TRUE)
    
    # # Link language button in main app and in the about window
    # observe({
    #   updateSelectInput(session, "language", selected = language_update())
    # })
    
    # Return the language code from the module
    return(reactive(input$language))
  })
}
    
## To be copied in the UI
# mod_language_ui("language")
    
## To be copied in the server
# mod_language_server("language")
