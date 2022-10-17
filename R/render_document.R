#' Helper functions to generate the RMD file
#' 
#' @description 
#' These functions help to generate the output rmarkdown file containing the filled out
#' Transparency Checklist as a string without a template on the fly.
#'
#' @name render_document
#' @aliases NULL
NULL

#' @rdname render_document
composeRmd <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, language_code = NULL, save_as = "pdf", short_checklist = NULL){
  # Get subtitle based on short or long checklist
  subtitle <- ifelse(short_checklist, "Transparency Report 1.0 (short, 12 items)", "Transparency Report 1.0 (full, 36 items)")
  # First, we create the YAML header of the rmd file (be carefully about indentation, can automatically generate another header which screws everything)
  headYaml <- stringr::str_glue(
"---
title: '{study_title}'
subtitle: '{sub_title}'
author: '{author_names}'
date: '{format(Sys.time(), '%d/%m/%Y')}'
header-includes:
  - \\usepackage{{ctex}}
  - \\setCJKmainfont{{Noto Serif CJK SC}}
  - \\usepackage{{fontspec}}
  - \\setmainfont{{FreeSerif}}
  - \\newfontfamily\\arabicfont{{FreeSerif}}
  - \\newfontfamily\\cyrillicfont{{FreeSerif}}
  - \\newfontfamily\\hebrewfont{{FreeSerif}}
  - \\newfontfamily\\greekfont{{FreeSerif}}
  - \\newfontfamily\\hangulfont{{Noto Serif CJK KR}}
lang: {language_code}
output: 
  {paste0(save_as, '_document')}:
    {ifelse(save_as == 'pdf', 'latex_engine: xelatex', 'default')}
babel-lang: chinese-simplified
---

{corr_author_label}: [{corresponding_email}]({corresponding_email})
  
{link_label}: [{link_to_rep}]({link_to_rep})
",
save_as = save_as,
study_title = ifelse(answers$studyTitle == "", server_translate("Untitled", language_code), answers$studyTitle),
sub_title = server_translate(subtitle, language_code),
author_names =  answers$authorNames,
corresponding_email = answers$correspondingEmail,
link_to_rep = answers$linkToRepository,
language_code = language_code,
corr_author_label = server_translate("Corresponding author's email address", language_code),
link_label = server_translate("Link to Project Repository", language_code)
)
  
  # fill in answers with "not answered" - important for generating the files
  bundleQuestions <- getItemList(sectionsList)
  not.answered <- !bundleQuestions %in% names(answers)
  notAnsweredLabel <- server_translate("Not answered", language_code)
  answers[bundleQuestions[not.answered]] <- notAnsweredLabel
  
  # We create sections of the rmd file
  sections <- sapply(sectionsList, composeSections, answers = answers, language_code = language_code, save_as = save_as)
  
  references <- renderReferences(language_code = language_code)
  # combine everything together
  rmd <- paste(c(headYaml, sections, references), collapse = "\n")
  
  # print created document for testing purposes
  # print(rmd)
  
  rmd
}

#' @rdname render_document
composeSections <- function(section, answers = NULL, language_code = NULL, save_as){
  # Creating a section
  # \\section{&SectionName}
  # First, we sketch the outline of the section
  body <- stringr::str_glue(
"

## &SectionName

**&SectionLabel**


&Questions

{ifelse(save_as == 'pdf', '\\newpage', '***')}
",
save_as = save_as
)
  # Generate the individual questions and their answers
  questions <- sapply(section$Questions, composeQuestions, answers = answers, language_code = language_code, save_as)
  
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

#' @rdname render_document
composeQuestions <- function(question, answers = answers, language_code = NULL, save_as){
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
 
  # make answers bold, but if it is a comment, show it as a quote
  if( !(question$Type %in% c("comment", "text"))){
    # If the response is NA we do not translate it
    resp <- ifelse(
      answers[[question$Name]] == "NA",
      answers[[question$Name]],
      server_translate(answers[[question$Name]], language_code)
    )
    
    # Change syntax based on output format
    answer <- stringr::str_glue(" {ifelse(save_as == 'pdf', '&escape&textbf{', '**')}{resp}{ifelse(save_as == 'pdf', '}', '**')} ")
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
    label <- stringr::str_glue(" {server_translate(question$Label, language_code)} {ifelse(save_as == 'pdf', '&escape&hfill', '')}")
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

#' @rdname render_document
renderReferences <- function(language_code = NULL){
out <- "
## &Refs
 
Aczel, B., Szaszi, B., Sarafoglou, A. Kekecs, Z., Kucharský, Š., Benjamin, D., ... & Wagenmakers, E.-J. (2019). A consensus-based transparency checklist. *Nature Human Behaviour*, 1--3. doi:10.1038/s41562-019-0772-6
"

  gsub("&Refs", server_translate("References", language_code), out)
}