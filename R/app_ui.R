#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Get UI elements
  checklist <- golem::get_golem_options("checklist")
  
  # Is this the short checklist?
  short_checklist <- golem::get_golem_options("short_checklist")
  
  # Set UI elements dependent on which checklist do we use
  if (short_checklist) {
    app_title <- "CREATING TRANSPARENCY CHECKLIST 1.0 (short, 12 items)"
    other_checklist <- "I prefer to fill out the full (36-item) checklist."
    link_other_checklist <- "http://www.shinyapps.org/apps/TransparencyChecklist/"
  } else {
    app_title <- "CREATING TRANSPARENCY CHECKLIST 1.0 (full, 36 items)"
    other_checklist <- "I prefer to fill out the short (12-item) checklist."
    link_other_checklist <- "http://www.shinyapps.org/apps/ShortTransparencyChecklist/"
  }
  
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
               with_i18n(app_title, NULL),
               align = "center"
               ),
        windowTitle = "Transparency Checklist 1.0"
        ),
      fluidRow(
        column(1),
        column(10,
               with_i18n(
                 tags$a(
                   other_checklist,
                   href = link_other_checklist,
                   target = "_blank"
                   ),
                 other_checklist
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
          h3("Please select an answer for each item below. If you want to elaborate on your answers, you can do so in the comment box that follows each section.") |> with_i18n(NULL)
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
      br(),
      br(),
      # info modal
      mod_intro_ui("intro"),
      # Select language
      absolutePanel(
        mod_language_ui("language"),
        top = "3%", right = "2%", fixed = TRUE, width = "10%"
      )
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
  )
}

