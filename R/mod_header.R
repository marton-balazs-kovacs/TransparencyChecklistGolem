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
    
    # TODO: shinyFeedback does not show ok icon for email and link, feedback text cannot be translated with with_i18n 
    
    # validate e-mail
    observeEvent(input$correspondingEmail, {
      email_validation <- isValidEmail(input$correspondingEmail)
      
        shinyFeedback::feedbackSuccess(
          inputId   = "correspondingEmail",
          show      = email_validation == TRUE && input$correspondingEmail != "",
          text      = NULL,
          color     = "black",
        )
        
        # "Provided email appears invalid."
        shinyFeedback::feedbackWarning(
          inputId   = "correspondingEmail",
          show      = email_validation == FALSE && input$correspondingEmail != "",
          text      = NULL,
          color     = "black"
        )
    })

    # validate link
    observeEvent(input$linkToRepository, {
      link_valid <- RCurl::url.exists(input$linkToRepository)

        shinyFeedback::feedbackSuccess(
          inputId   = "linkToRepository",
          show      = link_valid == TRUE && input$linkToRepository != "",
          text      = " ",
          color     = "black"
        )
        
        # "The link cannot be accessed."
        shinyFeedback::feedbackWarning(
          inputId   = "linkToRepository",
          show      = link_valid == FALSE && input$linkToRepository != "",
          text      = NULL,
          color     = "black"
        )
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
