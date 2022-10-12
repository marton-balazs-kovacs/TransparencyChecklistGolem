#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      shinyWidgets::dropdown(
        h4('Generate and Download Report') |> with_i18n(NULL),
        shinyWidgets::pickerInput(
          inputId = NS(id, "save_as"),
          label = "Format",
          choices = c("pdf", "html", "word", "rtf"),
          multiple = FALSE, width = 'auto', inline = FALSE
          ) |> with_i18n(
            "Format",
            selector = "label",
            attribute = "text"
            ),
        div(
          style = "display:inline-block",
          # Open window for a preview
          shinyWidgets::actionBttn(
            inputId = NS(id, "preview"), 
            label = "Preview",
            icon = icon("eye"),
            style = "simple",
            color = "primary",
            size = "xs",
            no_outline = FALSE
            ) |> with_i18n("Preview"),
          # Open window for a code
          shinyWidgets::actionBttn(
            inputId = NS(id, "showcode"),
            label = "Show code",
            icon = icon("code"),
            style = "simple",
            color = "primary",
            size = "xs",
            no_outline = FALSE
            ) |> with_i18n("Show code")
          ),
        br(),
        br(),
        div(
          id = "download-btn-div",
          # Show tooltip which says that the download is not ready
          title = "A report can be downloaded after all questions in each section have been answered.",
          # Download btn
          downloadButton(
            NS(id, 'report'),
            'Download',
            class = "downbutt"
            ) |> with_i18n("Download")
        ) |> with_i18n("A report can be downloaded after all questions in each section have been answered.", attribute = "title"),
        icon = icon("file-lines"),
        up = TRUE,
        style = "unite",
        label = with_i18n("Generate Report", NULL),
        size = "lg",
        inputId = NS(id, "generatereport"),
        width = "20vw",
        class = "fixedButton"
        ),
      # Tooltip for the "Generate Report" btn
      title = "Click here to create and download report",
      bottom = "2.5%",
      left = "50%",
      fixed = TRUE,
      width = "auto",
      style = "transform: translate(-50%, +0%); z-index: 1000;"
      # Translate the tooltip
      ) |> with_i18n("Click here to create and download report", attribute = "title"),
  )
}
    
#' report Server Functions
#'
#' @noRd 
mod_report_server <- function(id, checklist, answers, language_code){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # checks which sections are complete
    whichComplete <- reactive({
      # Currently the headers do not have to be filled out in order to download a checklist
      isComplete(
        answers = answers(),
        sectionsList = checklist$sectionsList,
        headList = checklist$headList
        )
    })

    # checks whether the report is complete
    isDownloadable <- reactive({
      all(whichComplete())
    })

    #### Reactive animations ----
    # whenever the input is complete, let's enable the download button,
    # show code button and show preview button
    observe({
      if(isDownloadable()) {
        shinyjs::enable("report")
        shinyjs::enable("showcode")
        shinyjs::enable("preview")
        golem::invoke_js("remove_tooltip", "#download-btn-div")
        # and start animation every 4 sec
        invalidateLater(4000, session)
        shinyanimate::startAnim(session, "generatereport", type = "bounce")
      } else {
        shinyjs::disable("report")
        shinyjs::disable("showcode")
        shinyjs::disable("preview")
        golem::invoke_js("add_tooltip",
                         list(
                           where = "#download-btn-div",
                           message = "A report can be downloaded after all questions in each section have been answered."))
      }
    })
    
    # Toggle color of checker icons for questions
    observeEvent(input$generatereport, {
      # Get ids of items from internal data
      items <- getItemList(checklist$sectionsList, all = FALSE)
      # Get responses
      ans   <- isolate(answers())
      
      for(item in items) {
        if(ans[item] == "" || is.null(ans[[item]])) {
          shinyanimate::startAnim(
            session,
            # Add namespace to item ids
            paste0("sections-", item, "Checker"),
            type = "shake"
            )
        }

        session$sendCustomMessage(
          type = "toggleCheckerColor",
          message = list(
            # namespacing of a different module
            # think of an alternative solution later
            id = paste0("sections-", item, "Checker"),
            val = ans[[item]],
            # namespacing of a different module
            divId = paste0("sections-", "div", item, "Checker"))
        )
      }
    })

    # Change icons in Section headings (three state option)
    observe({
      sectionValues <- sapply(checklist$sectionsList, function(sec) sec$Value)
      for(i in seq_along(sectionValues)){
        session$sendCustomMessage(
          type = "toggleSectionIcon",
          # as long as the user does not click "report", do not display aggresive feedback (-> val = "init")
          message = list(
            id = paste0(".icon", sectionValues[[i]]),
            val = ifelse(
              input$generatereport == 0 && !whichComplete()[[i]],
              "init",
              whichComplete()[[i]]
              )
          )
        )
      }
    })

    #### Working with report ----
    # Stash current Rmd if report dropdown is opened or save_as is changed
    RmdFile <- reactive({
      dontrun <- input$generatereport
      composeRmd(
        answers = isolate(answers()),
        sectionsList = checklist$sectionsList,
        headList = checklist$headList,
        answerList = checklist$answerList,
        save_as = input$save_as,
        language_code = isolate(language_code())
        )
    })

    # render Rmd file in show code modal panel
    output$code <- renderText({
      RmdFile()
    })
    
    generateCode <- function() {
      modalDialog(
        shinycssloaders::withSpinner(verbatimTextOutput(ns("code"))),
        easyClose = TRUE,
        footer = NULL
      )
    }
    
    # Show code modal
    observeEvent(input$showcode, {
      showModal(generateCode())
    })
    
    # Render previews
    generatePreview <- function(failed = FALSE) {
      RmdPath <- file.path(tempdir(), "report.Rmd")
      writeLines(RmdFile(), con = RmdPath)
      
      if(input$save_as %in% c("word", "rtf")){
        modalDialog(
          showNotification(with_i18n("Word and rtf files cannot be previewed in the browser, displaying markdown file", NULL),
                           type = "warning", closeButton = FALSE, duration = 7),
          includeMarkdown(RmdPath),
          easyClose = TRUE
        )
      } else{
        # save_as <- ifelse(input$save_as == "word", "docx", input$save_as)
        # out_file <- paste0("preview.", input$save_as)
        out_file <- fs::file_temp("preview", tmp_dir = app_sys("app/www/doc"), ext = paste0(".", input$save_as))
        
        rmarkdown::render(RmdPath, output_file = out_file,
                          # output_dir = app_sys("app/www/doc"),
                          envir = new.env(parent = globalenv())
                          )
        # src_file <- file.path("www/doc", out_file)
        src_file <- stringr::str_extract(out_file, "www/doc/\\w+.\\w+")
  
        modalDialog(
          shinycssloaders::withSpinner(tags$iframe(style = "height:600px; width:100%", src = src_file)),
          easyClose = TRUE,
          footer = NULL
        )
      }
    }
    
    # Show preview modal
    observeEvent(input$preview, {
      showModal(generatePreview())
      # Trigger translate notification
      localize(".shiny-notification-content-text")
    })

    #### Download ----
    # This section deals with the pdf generation
    output$report <- downloadHandler(

      filename = function() {
        save_as <- ifelse(input$save_as == "word", "doc", input$save_as)
        paste("Transparency Report", save_as, sep = ".")
      },

      content = function(file) {
        # Create the report file in a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).

        # create a string which copies the structure of the desired rmd file
        RmdFile <- composeRmd(
          answers = answers(),
          sectionsList = checklist$sectionsList,
          headList = checklist$headList,
          answerList = checklist$answerList,
          save_as = input$save_as,
          language_code = language_code()
          )

        # print the Rmd document in the console (for debugging)
        #writeLines(RmdFile)

        # store the string as a temporary report.Rmd file
        tempReport <- file.path(tempdir(), "report.Rmd")
        writeLines(RmdFile, con = tempReport)

        # knit the temporary document into a proper pdf (which will be called "report.pdf/html/doc")
        rmarkdown::render(tempReport, output_file = file,
                          envir = new.env(parent = globalenv()))

        showNotification("Downloaded", type = "message", duration = 3, closeButton = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_report_ui("report")
    
## To be copied in the server
# mod_report_server("report")
