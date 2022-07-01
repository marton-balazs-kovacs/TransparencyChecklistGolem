#' Translatable widgets
#' 
#' @description Some widgets are not translatable by the current implementation if i18n in Shiny.
#' These functions hard code html for these widgets with translatable options.
#' 
NULL

#' blabla
#' 
#' @description shinyWidgets::pickerInput
pickerInputTranslatable <- function(inputId, choices) {
  options <- list()
  for(i in seq_along(choices)) {
    options[[i]] <- tags$option(value = choices[i], names(choices[i])) |> with_i18n(names(choices[i]))
  }
  
  div(
    class = "form-group shiny-input-container",
    tags$label(
      class = "control-label",
      `for` = inputId
    ),
    tags$select(
      id = inputId,
      class = "form-control",
      HTML('<option value="" disabled selected data-i18n="Please select an option">Please select an option</option>'),
      options
    )
  )
}

#' @description This function hard codes html for shiny::radioButton with translatable options 
radioButtonTranslatable <- function(inputId, choices) {
  div(id = inputId, class = "form-group shiny-input-radiogroup shiny-input-container", role = "radiogroup", `aria-labelledby` = sprintf("%s-label", inputId),
      tags$label(class = "control-label", id = sprintf("%s-label", inputId), `for` = inputId),
      radioButtonTranslatableOptions(choices, inputId))
}

radioButtonTranslatableOptions <- function(choices, inputId) {
  options <- list()
  for(i in seq_along(choices)) {
    options[[i]] <- radioButtonTranslatableOption(choices[i], inputId)
  }
  
  div(class = "shiny-options-group", options)
}

radioButtonTranslatableOption <- function(choice, inputId) {
  div(class = "radio-inline",
      tags$input(type = "radio", name = inputId, value = choice),  
      # |> with_i18n(
      #   names(choice),
      #   attribute = "value"
      #   ),
      tags$label(
        names(choice),
        `for` = names(choice)
      ) |> 
        with_i18n(names(choice))
      
  )
}

#' @description This function hard codes html for shiny::textArea with translatable options
textAreaInputTranslatable <- function(inputId, placeholder, rows) {
  div(class = "form-group shiny-input-container",
      tags$label(class = "control-label", id = inputId, `for` = inputId),
      tags$textarea(class = "form-control", id = inputId, placeholder = "bla", rows = rows)
  )
}