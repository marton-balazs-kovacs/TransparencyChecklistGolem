#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      shinyWidgets::actionBttn(inputId = ns("trigger"), label = with_i18n("About", "About"), icon = icon("circle-info")),
      top = "3%", left = "2%",
      # fixed = TRUE,
      width = "auto"
    )
  )
}
    
#' intro Server Functions
#'
#' @noRd 
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Content of the intro modal
    intro_modal <- function() {
      modalDialog(
        # fluidRow(
        #   column(8),
        #   column(4,
        #          selectInput(NS(id, "intro_language"), "Select Language", language_list, width = "auto") |> 
        #            with_i18n("Select Language", selector = "label")
        #          )
        #   ),
        withTags({
          div(id = "intro-modal-content",
            h3("What is the Transparency Checklist?") |> with_i18n(NULL),
            p("The Transparency Checklist is a comprehensive checklist that researchers can use to improve and document the transparency of their research. This checklist was developed for social and behavioral scientists who conduct and report confirmatory research on primary data. Nevertheless, several of the checklist items may also be relevant for other approaches and disciplines. For purely exploratory research, only the last 5 items of this short checklist apply.") |> with_i18n(NULL),
            br(),
            h3("How to use it?") |> with_i18n(NULL),
            ul(
              li("The checklist refers to a single study of interest.") |> with_i18n(NULL),
              li("Please respond to each checklist item. If necessary, you can provide an explanation at the end of each section.") |> with_i18n(NULL),
              li("When the question refers to your manuscript, this includes all supplementary materials and appendices that are relevant to the study of interest.") |> with_i18n(NULL),
              li("After all the questions have been answered, you can generate a transparency report for your study by pressing the button labeled GENERATE REPORT at the bottom of the page.") |> with_i18n(NULL),
              li("Save your transparency report on your computer. Note that after you download your report, your responses on the checklist will not be saved by our webpage.") |> with_i18n(NULL),
              li("Upload your transparency report to a public repository.") |> with_i18n(NULL),
            ),
            br(),
            span("You can cite the Transparency Checklist as follows:") |> with_i18n(NULL),
            br(),
            p("Aczel, B., Szaszi, B., Sarafoglou, A. Kekecs, Z., Kucharský, Š., Benjamin, D., ... & Wagenmakers, E.-J. (2019). A consensus-based transparency checklist.",
              i("Nature Human Behaviour, "), "1--3.",
              a("doi:10.1038/s41562-019-0772-6", href = "https://doi.org/10.1038/s41562-019-0772-6", target = "_blank")),
            hr(),
            p(with_i18n("Feedback and recommendations for an update of the checklist can be provided here:", NULL),
              a("https://forms.gle/raN7q1ucpov5sX316", href = "https://forms.gle/raN7q1ucpov5sX316", target = "_blank")),
            br(),
            a(
              img(src = "www/GitHub-Mark-32px.png"),
              href = "https://github.com/BalazsAczel/TransparencyChecklist",
              target = "_blank"
            )
          )
        }),
        easyClose = TRUE,
        footer = NULL
      )
    }
    
    # Open intro modal
    observeEvent(input$trigger, {
      showModal(intro_modal())
      localize("#intro-modal-content")
    })
    
    # # Link language button in main app and in the about window
    # observe({
    #   updateSelectInput(session, "intro_language", selected = language_code())
    # })
    # 
    # # Return the language set in the intro modal so the other language select input can be updated
    # return(reactive(input$intro_language))
  })
}
    
## To be copied in the UI
# mod_intro_ui("intro")
    
## To be copied in the server
# mod_intro_server("intro")
