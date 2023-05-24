#' with_i18n
#'
#' @description Add i18n to the tag. If character value is provided instead
#' of a tag, the function will use span tag by default.
#'
#' @return An HTML tag with data-i18n added.
#' @param tag An HTML tag.
#' @param i18n The i18n entry.
#'
#' @example with_i18('Hello World!', 'hello-world')
#'
#' @noRd
with_i18n <- function(
  tag,
  i18n,
  attribute = NULL,
  selector = NULL
) {
  # Use span tag if no tag provided
  if (class(tag)[1] == "character") {
    tag <- htmltools::tags$span(tag)
  }
  
  # Guess and use the character string in tag as i18n if no i18n is provided
  if (is.null(i18n)) {
    i18n <- purrr::detect(tag$children, is.character)
    # Check for failed guess
    if (is.null(i18n)) stop("Please provide a key as a string for the translation in the i18n parameter!")
  }
  
  # Remove semicolon from the i18n code automatically
  # Not a generalized solution but an easy DIV for our case
  if (stringr::str_detect(i18n, ";")) {
    i18n <- stringr::str_remove_all(i18n, ";")
  }
  
  # Append translator data attribute
  htmltools::tagAppendAttributes(
    tag,
    `data-i18n` = ifelse(
      is.null(attribute),
      i18n,
      paste0("[", attribute, "]", i18n)),
    .cssSelector = selector
  )
}

#' localize
#'
#' @description Launch a localize on an element
#'
#' @return Used for side-effect
#'
#' @param element The element to localize()
#' @param session Shiny session object
#'
#' @example localize("#nav")
#'
#' @noRd
localize <- function(
  element = "html",
  session = shiny::getDefaultReactiveDomain()
) {
  golem::invoke_js(
    "localize",
    list(
      element = element
    )
  )
}

#' Change language
#'
#' @description Set the i18n language
#'
#' @return Used for side-effect
#'
#' @param lang The language to used in i18next.changeLanguage();
#' @param session Shiny session object
#'
#' @example change_language("fr")
#'
#' @noRd
change_language <- function(
  lang = "en",
  session = shiny::getDefaultReactiveDomain()
) {
  golem::invoke_js(
    "changeLanguage",
    list(
      lang = lang
    )
  )
}

#' Get language
#'
#' @description Get the i18n language
#'
#' @return Used for side-effect
#'
#' @param id The shiny id to get the language
#' @param session Shiny session object
#'
#' @example get_language()
#'
#' @noRd
get_language <- function(
  id = "language",
  session = shiny::getDefaultReactiveDomain()
) {
  golem::invoke_js(
    "Language",
    list(
      id = id
    )
  )
}

#' Server side translator
#' 
#' @description This is a simple function for doing server side translations.
#' Not an ideal solution but I do not want to use shiny.i18n on top of the gemstones' functions.
#' 
#' @noRd
server_translate <- function(i18n, language_code = NULL) {
  # Get the JSON containing localization
  local <- rjson::fromJSON(file = app_sys("app/www/i18n_locales.json"))
  
  # Filter by language code
  lang <- purrr::pluck(local, language_code, "translation")
  
  # Remove semicolon from the i18n code automatically
  # Not a generalized solution but an easy DIV for our case
  if (stringr::str_detect(i18n, ";")) {
    i18n <- stringr::str_remove_all(i18n, ";")
  }
  
  # Get translation by key
  translation <- purrr::pluck(lang, i18n)
  
  # Validate result
  if (is.null(translation)) {
    stop(paste("Can't find the key in the local JSON:", i18n))
  } else {
    return(translation)
  }
}

#' Transform dataframe to list
#' 
#' @param x a data frame with translations in rows and languages in columns
#' 
#' @returns a list of length 2 with x[[1]] has the vector of languages and x[[2]] has the translations
df2list <- function(x){
  out <- list()
  out[['languages']] <- as.list(colnames(x))
  out[['translation']] <- list()
  
  for(i in 1:nrow(x)){
    out[['translation']][[i]] <- as.list(x[i,])
  }
  
  return(out)
}
