#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Get UI elements
  checklist <- golem::get_golem_options("checklist")
  
  # Generate sections
  section_answers <- mod_sections_server("sections", checklist = checklist)
  
  # Generate header
  header_answers <- mod_header_server("header", checklist = checklist)
  
  # Languages
  language_code <- mod_language_server("language")
  
  # Intro
  mod_intro_server("intro")
  
  # Generate report
  mod_report_server(
    "report",
    checklist = checklist,
    answers = reactive({c(header_answers(), section_answers())}),
    language_code = language_code
    )
}
