#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Get UI elements
  checklist <- golem::get_golem_options("checklist")
  
  # UI
  tagList(
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      theme = shinythemes::shinytheme("cerulean"),
      # Application title
      headerPanel(
        column(12,
               "CREATING TRANSPARENCY CHECKLIST 1.0 (full, 36 items)",
               align = "center"
               ),
        windowTitle = "Transparency Checklist 1.0"
        ) |> with_i18n("CREATING TRANSPARENCY CHECKLIST 1.0 (full, 36 items)", selector = "h1"),
      fluidRow(
        column(1),
        column(10,
               with_i18n(
                 tags$a(
                   "I prefer to fill out the short (12-item) checklist.",
                   href = "http://www.shinyapps.org/apps/ShortTransparencyChecklist/",
                   target = "_blank"
                   ),
                 "I prefer to fill out the short (12-item) checklist."
                 ),
               align = "center"
               ),
        column(1)
      ),
      br(),
      # The header (basic information about the paper and authors)
      mod_header_ui("header", checklist = checklist),
      # Show initial instructions
      fluidRow(
        column(1),
        column(
          10,
          h3("Please select an answer for each item below. If you want to elaborate on your answers, you can do so in the comment box that follows each section.")
        ),
        column(1)
      ),
      br(),
      br(),
      tags$div(id = "scrollAnchor"), # for scrolling up
      # Show questions
      mod_sections_ui("sections", checklist = checklist),
      # Switching between sections
      br(),
      br(),
      # Report menu (downloading)
      mod_report_ui("report"),
      # Show tooltip which says that the download is not ready
      # shinyBS::bsTooltip(id = "report",
      #                    title = "A report can be downloaded after all questions in each section have been answered.",
      #                    # Please, respond to all displayed items to download the pdf report (comments are optional).
      #                    trigger = "manual",
      #                    placement = "right"),
      # uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
      br(),
      br(),
      # info modal
      mod_intro_ui("intro"),
      # Select language
      mod_language_ui("language")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'TransparencyChecklistGolem'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(), # this is for enabling/disabling buttons from shinyjs
    shinyFeedback::useShinyFeedback(), # enabling/disabling feedback from shinyFeedback
    shinyanimate::withAnim(), # enable animations from shinyanimate
    # # Add custom css stylesheet
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    # tags$script(src = "toggleChecker.js"),
    # tags$script(src = "toggleCheckerColor.js"),
    # tags$script(src = "toggleSectionIcon.js")
  )
}

