#' sections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sections_ui <- function(id, checklist){
  ns <- NS(id)
  
  sectionsList <- checklist$sectionsList
  
  sectionsHTML <- lapply(sectionsList, renderSection, id = id)
  names(sectionsHTML) <- NULL
  sectionsHTML <- do.call(tabsetPanel, c(sectionsHTML, id = NS(id, "sections")))
  
  tagList(
    sectionsHTML,
    fluidRow(
      column(2),
      column(2, align = "center",
             actionButton(
               ns("previousButton"),
               with_i18n("Go to previous section", "Go to previous section"),
               icon = icon("circle-arrow-left", lib = "font-awesome")
               )
      ),
      column(4),
      column(2, align = "center",
             actionButton(
               ns("nextButton"),
               with_i18n("Go to next section", "Go to next section"),
               icon = icon("circle-arrow-right")
               )
      ),
      column(2)
    )
  )
}
    
#' sections Server Functions
#'
#' @noRd 
mod_sections_server <- function(id, checklist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    sectionsList <- checklist$sectionsList
    
    #### Moving to the next or previous sections ----
    observeEvent(input$nextButton, {
      sectionId <- sapply(sectionsList, function(section) section$Value)
      currSection <- which(sectionId == input$sections)
      nextSection <- currSection + 1
      updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
      shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
    })
    
    observeEvent(input$previousButton, {
      sectionId <- sapply(sectionsList, function(section) section$Value)
      currSection <- which(sectionId == input$sections)
      nextSection <- currSection - 1
      updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
      shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
    })
    
    # disable moving next or previous for first and last sections
    observeEvent(input$sections, {
      sectionId <- sapply(sectionsList, function(section) section$Value)
      currSection <- which(sectionId == input$sections)
      if(currSection == 1){
        shinyjs::hide("previousButton")
      } else{
        shinyjs::show("previousButton")
      }
      
      if(currSection == length(sectionId)){
        shinyjs::hide("nextButton")
      } else{
        shinyjs::show("nextButton")
      }
    })
    
    # changing icons when item is answered
    # change id to fit module
    observe({
      items <- getItemList(sectionsList, all = FALSE) # loop only on items

      for(item in items){
        session$sendCustomMessage(
          type = "toggleChecker",
          message = list(
            id = ns(paste0(item, "Checker")),
            val = input[[item]],
            divId = ns(paste0("div", item, "Checker"))
            )
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
# mod_sections_ui("sections")
    
## To be copied in the server
# mod_sections_server("sections")
