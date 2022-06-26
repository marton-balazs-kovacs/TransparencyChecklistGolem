#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_header_ui <- function(id, checklist){
  ns <- NS(id)
  headList <- checklist$headList
  
  html <- lapply(headList, switchButtons, id = id)
  
  tagList(
    fluidRow(
      column(1),
      column(10,
             wellPanel(html)
      ),
      column(1)
    )
  )
}
    
#' header Server Functions
#'
#' @noRd 
mod_header_server <- function(id, checklist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    headList <- checklist$headList
    
    # stores the answers in a list
    answers <- reactive({
      reactiveValuesToList(input)
    })
    
    # validate title of the study
    observeEvent(input$studyTitle, {
      shinyFeedback::feedbackSuccess(
        inputId = "studyTitle",
        show    = input$studyTitle != "",
        text    = NULL,
        color   = "black"
      )
    })
    
    # validate author names
    observeEvent(input$authorNames, {
      shinyFeedback::feedbackSuccess(
        inputId = "authorNames",
        show    = input$authorNames != "",
        text    = NULL,
        color   = "black"
      )
    })
    
    
    # validate e-mail
    # For some reason not working
    observeEvent(input$correspondingEmail, {
      if (input$correspondingEmail == "@" || input$correspondingEmail == ""){
        shinyFeedback::feedback(
          inputId   = "correspondingEmail",
          show      = TRUE,
          text      = " ",
          color     = "black",
          icon      = NULL
        )
      } else if (isValidEmail(input$correspondingEmail)){
        shinyFeedback::feedbackSuccess(
          inputId   = "correspondingEmail",
          show      = TRUE,
          text      = " ",
          color     = "black"
        )
      } else {
        shinyFeedback::feedbackWarning(
          inputId   = "correspondingEmail",
          show      = TRUE,
          text      = "Provided email appears invalid.",
          color     = "black"
        )
      }
    })
    
    # validate link
    # For some reason not working
    observeEvent(input$linkToRepository, {
      if (input$linkToRepository == ""){
        shinyFeedback::feedback(
          inputId   = "linkToRepository",
          show      = TRUE,
          text      = " ",
          color     = "black",
          icon      = NULL
        )
      } else if (RCurl::url.exists(input$linkToRepository)){
        shinyFeedback::feedbackSuccess(
          inputId   = "linkToRepository",
          show      = TRUE,
          text      = " ",
          color     = "black"
        )
      } else {
        shinyFeedback::feedbackWarning(
          inputId   = "linkToRepository",
          show      = TRUE,
          text      = "The link cannot be accessed.",
          color     = "black"
        )
      }
    })
    
    # stores the answers in a list
    answers <- reactive({
      reactiveValuesToList(input)
    })
    
    # return answers
    return(answers)
  })
}
    
## To be copied in the UI
# mod_header_ui("header")
    
## To be copied in the server
# mod_header_server("header")
