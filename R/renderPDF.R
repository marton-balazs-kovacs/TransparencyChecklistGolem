## functions to generate the rmd file--
composeRmd <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL, save_as = "pdf"){
  # returns a string

  switch (save_as,
    "pdf" = composePDF(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code),
    "html" = composeHTML(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code),
    "word" = composeDOC(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code),
    "rtf" = composeRTF(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code)
  )
}

composeHTML <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code)
  rmd <- gsub("pdf_document", "html_document", rmd)
  
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  
  return(rmd)
}

composeDOC <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code)
  rmd <- gsub("pdf_document", "word_document", rmd)
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  return(rmd)
}

composeRTF <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList, language_code = language_code)
  rmd <- gsub("pdf_document", "rtf_document", rmd)
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  return(rmd)
}


## functions to generate the rmd file--
composePDF <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL){
  # returns a string
  
  # First, we create the YAML header of the rmd file (be carefull about indentation, can automatically generate another header which screws everything)
  headYaml <- 
"---
title: '&studyTitle'
subtitle: '&subTitle'
author: '&authorNames'
date: '&date'
header-includes:
  - \\usepackage{ctex}
  - \\newfontfamily\\arabicfont{Times New Roman}
  - \\newfontfamily\\cyrillicfont{Times New Roman}
  - \\newfontfamily\\hebrewfont{Times New Roman}
  - \\newfontfamily\\greekfont{Times New Roman}
output: 
  pdf_document:
    latex_engine: xelatex
lang: &languageCode
---
  
&corrAuthorsLabel: [&correspondingEmail](&correspondingEmail)
  
&linkToRepoLabel: [&linkToRepository](&linkToRepository)
"

  
  # and fill the header with information taken from the question in the head
  date <- format(Sys.time(), '%d/%m/%Y')
  answers$studyTitle <- ifelse(answers$studyTitle == "", server_translate("Untitled", language_code), answers$studyTitle)

  headYaml <- gsub("&studyTitle",         answers$studyTitle,                                  headYaml)
  headYaml <- gsub("&authorNames",        answers$authorNames,                                 headYaml)
  headYaml <- gsub("&correspondingEmail", answers$correspondingEmail,                          headYaml)
  headYaml <- gsub("&linkToRepository",   answers$linkToRepository,                            headYaml)
  headYaml <- gsub("&date",               date,                                                headYaml)
  headYaml <- gsub("&languageCode",       language_code,                   headYaml)
  headYaml <- gsub("&subTitle",           server_translate("Transparency Report 1.0 (full, 36 items)", language_code),  headYaml)
  headYaml <- gsub("&corrAuthorsLabel",   server_translate("Corresponding author's email address", language_code),      headYaml)
  headYaml <- gsub("&linkToRepoLabel",    server_translate("Link to Project Repository", language_code),                headYaml)
  
  # fill in answers with "not answered" - important for generating the files
  bundleQuestions <- getItemList(sectionsList)
  not.answered <- !bundleQuestions %in% names(answers)
  notAnsweredLabel <- server_translate("Not answered", language_code)
  answers[bundleQuestions[not.answered]] <- notAnsweredLabel
  
  # We create sections of the rmd file
  sections <- sapply(sectionsList, composeSections, answers = answers, language_code = language_code)
  
  references <- renderReferences(language_code = language_code)
  # combine everything together
  rmd <- paste(c(headYaml, sections, references), collapse = "\n")
  
  rmd
}

composeSections <- function(section, answers = NULL, language_code = NULL){
  # Creating a section
  # \\section{&SectionName}
  # First, we sketch the outline of the section
  body <- 
"

## &SectionName

**&SectionLabel**


&Questions

\\newpage
"

  # Generate the individual questions and their answers
  questions <- sapply(section$Questions, composeQuestions, answers = answers, language_code = language_code)
  
  # Fill in the section Name, the text, and the generated questions
  body <- gsub("&SectionName", server_translate(section$Name, language_code), body)
  if(is.null(section$Label) || section$Label == ""){
    body <- gsub("\\*\\*&SectionLabel\\*\\*", "", body)
  } else{
    body <- gsub("&SectionLabel", server_translate(section$Label, language_code), body)
  }
  body <- gsub("&Questions", paste(questions, collapse = " \n"), body)
  
  # Escape latex backslashes from the question generation
  body <- gsub("&escape&", "\\", body, fixed = TRUE) # double escaping screws latex code

  body
}

composeQuestions <- function(question, answers = answers, language_code = NULL){
  # This function takes a question (from the .json file), checks whether it is supposed to be shown
  # (based on the answers and the conditional statements from .json)
  # If it is supposed to be shown, the question and its answer is printed


  show <- TRUE
  
  # check whether the section is supposed to be shown
  if(!is.null(question$Depends)){
    show <- gsub(".ind_", "answers$ind_", question$Depends)
    show <- eval(parse(text = show))
  }
  
  # if the question is not shown, return empty space (will screw up the appearance of the rmd file, but not the pdf)
  if(!show){
    return("")
  }
  
  body <- 
"
&Label &Answer
"
  
  
  # if the question is "Explain" -- additional comment following some question, render it as a comment
  if(question$Label == "Explain") {
    question$Type <- "comment"
  }
  print(question$Name)
  # make answers bold, but if it is a comment, show it as a quote
  if( !(question$Type %in% c("comment", "text"))){
    # If the response is NA we do not translate it
    resp <- ifelse(
      answers[[question$Name]] == "NA",
      answers[[question$Name]],
      server_translate(answers[[question$Name]], language_code)
    )
    
    answer <- paste0(" &escape&textbf{", resp, "} ")
  } else if(question$Type == "comment"){
    answer <- ifelse(answers[[question$Name]] == "", server_translate("No comments.", language_code), answers[[question$Name]]) # If the comment box is empty
    answer <- paste0("\n\n> ", answer)
  } else{
    answer <- ""
  }
  

  # layout Labels
  if(!is.null(question$href)){
    question$Label <- paste0(question$Label, "[", question$href, "](", question$href, ")")
  }
  if(!is.null(question$LabelEnd)){
    question$Label <- paste0(question$Label, question$LabelEnd)
  }
  
  if( !(question$Type %in% c("comment", "text"))){
    label <- paste0(" ", server_translate(question$Label, language_code), " &escape&hfill")
  } else if(question$Type == "text" || (question$Type == "comment" && question$Label != "Explain")){
    if(question$Label == ""){
      label <- paste0("\n")
    } else{
      label <- paste0("**", server_translate(question$Label, language_code), "**")
    }
  } else{
    label <- ""
  }
  
  body <- gsub("&Label", label, body)
  body <- gsub("&Answer", answer, body)

  return(body)
}

renderReferences <- function(language_code = NULL){
out <- "
## &Refs
 
Aczel, B., Szaszi, B., Sarafoglou, A. Kekecs, Z., Kucharský, Š., Benjamin, D., ... & Wagenmakers, E.-J. (2019). A consensus-based transparency checklist. *Nature Human Behaviour*, 1--3. doi:10.1038/s41562-019-0772-6
"

  gsub("&Refs", server_translate("References", language_code), out)
}