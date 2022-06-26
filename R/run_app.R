#' Run the Shiny Application
#'
#' @param short_checklist Whether to create the app for the long or the short checklist
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  short_checklist = FALSE
) {
  
  # Load the .json, which defines the structure of the application
  # TODO: change short checklist json accordingly
  if (short_checklist) {
    checklist <- NULL
  } else {
    checklist <- TransparencyChecklistGolem:::long
  }
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(
      checklist = checklist
    )
  )
}
