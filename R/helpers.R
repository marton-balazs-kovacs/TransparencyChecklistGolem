## Functions which help us to create some structure of the shiny app
## and defines the buttons which appear horizontal

renderSection <- function(section, id = NULL, answers = NULL){
  # creates a tab ( can be changed to a fluidrow if we do not want tabs)
  
  tabPanel(
    title = with_i18n(section$Name, section$Name), 
    value = section$Value, 
    icon  = tags$i(class = paste0("icon", section$Value, " fa fa-eye")),
    br(),
    # create the header and an initial info about the section
    fluidRow(column(1),
             column(10,
                    if(!is.null(section$Label)) {
                      strong(with_i18n(section$Label, section$Label))
                      }
                    ),
             column(1)),

    # render all fields within this section
    lapply(section$Questions, customField, id = id, answers = answers),
    
    # a break line after each section
    fluidRow(hr())
  )
  
}

customField <- function(ind, id = NULL, answers = NULL){
  # if the input is not a question, it is assumed that it is some guidance text in between the items
  if(ind$Type == "text"){
    
    # the guidance text can itself be conditional
    if(is.null(ind$Depends)){
      fluidRow(column(1),
               column(10, br(), strong(ind$Label)),
               column(1))
    } else{
      # Add module id
      depends <- dep_ns(ind$Depends, id = id)
      
      conditionalPanel(condition = depends,
                       fluidRow(column(1), 
                                column(10, br(), strong(ind$Label)),
                                column(1))
                       )
    }
  } else { # render questions
    customButton(ind, id = id, answers = answers)
  }
}


customButton <- function(ind, id = NULL, answers = NULL){
    
  # Always display unconditional items  
  if(is.null(ind$Depends)){
    ind$Depends <- "true"
  } else { # or display depending on the state of the input
    # Add module id
    ind$Depends <- dep_ns(ind$Depends, id = id)

  }
  
  if(ind$Type != "comment"){ # when the item is not a comment, show the button in a 6:3:1 format (label:button:validation)
    
    fluidPage( # wrapping into another fluid page makes a slight indentation of the questions from the text fields
    conditionalPanel(condition = ind$Depends,
                     fluidRow(column(1),
                              column(6, br(), with_i18n(ind$Label, ind$Label)),#, 
                                     #a(ind$href, href = ind$href, target = "_blank"),
                                     #ind$LabelEnd), # this makes the buttons appear horizontally aligned
                              # do not add ns here as it would duplicate ns tag in consequent calls
                              column(3, switchButtons(ind, id = id, answers = answers)), # create a standard shiny button
                              column(1, br(), # adds exclamation circle next to the item
                                     tags$div(
                                       id = shiny::NS(id, paste0("div", ind$Name, "Checker")),
                                       title = with_i18n("This question needs to be answered.", "This question needs to be answered."),
                                       tags$i(id = shiny::NS(id,  paste0(ind$Name, "Checker")),
                                              class = 'fa fa-exclamation-circle')
                                       )
                                     ),
                              column(1)
                     )
    )
    )
  } else { # when the item is a comment, show the commentary section as a standard textArea stretched over the width of the panel
      conditionalPanel(condition = ind$Depends,
                       fluidRow(column(1),
                                column(10, br(), strong(with_i18n(ind$Label, ind$Label)), br(),
                                       tags$style(type = "text/css", "textarea {width:80%}"),
                                       tags$textarea(ifelse(is.null(answers[[ind$Name]]), "", answers[[ind$Name]]),
                                                     id = shiny::NS(id, ind$Name), placeholder = ind$AnswerType,
                                                     rows = 5, class = "form-control") |> with_i18n(ind$AnswerType, attribute = "placeholder")
                                       ),
                                column(1)))
  }
}


switchButtons <- function(ind, id = NULL, answers = NULL){
  # Add module id
  ind$Name <- shiny::NS(id, ind$Name)
  
  # TODO: Add short and long transparency 
  answerList <- TransparencyChecklistGolem:::long$answerList
  # if the AnswerType is specified in the answerList object (from .json), the button options should be rendered from 
  # those options
  # otherwise, the AnswerType is passed directly to the options
  if(ind$AnswerType %in% names(answerList)){
    answerOptions <- answerList[[ind$AnswerType]]
  } else{ 
    answerOptions <- ind$AnswerType
  }
  
  # preserve selected values if translation was called
  answered <- ind$Name %in% names(answers)
  if(answered){
    selected <- answers[[ind$Name]]
  } else{
    selected <- NULL
  }
  
  # switch between different input types
  switch (ind$Type,
    "select"    = pickerInputTranslatable (inputId = ind$Name, choices = answerOptions),
    "radio"     = radioButtonTranslatable  (inputId = ind$Name, choices = answerOptions),
    "textInput" = textInput                (inputId = ind$Name, label = with_i18n(ind$Label, ind$Label)),
    "textArea"  = textAreaInputTranslatable(inputId = ind$Name, placeholder = answerOptions, rows = 6)
  )
}


getItemList <- function(sectionsList, all = TRUE, id = NULL){
  # Get list of question ids
  items <- unlist(
    sapply(sectionsList,
           function(section) {
             sapply(section$Questions, function(item) item$Name)
             }
           )
    )
  
  # Add module id
  items <- shiny::NS(id, items)
  
  if(all){
    return(items)
  } else {
    return(items[grep("ind", items)])
  }
}


# translateLabel <- function(label) {return(label)}

## Translatable widgets ----
#' @description This function hard codes html for shinyWidgets::pickerInput with translatable options
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