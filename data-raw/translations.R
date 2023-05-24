# Code to transform the translations from spreadsheets to JSON
# Load the translations from a google drive
# Loading pacakges
library(here)
library(readxl)
library(readr)
library(tibble)
library(jsonlite)
library(dplyr)

# The translations are stored in a private google drive
# We downloaded them and unzipped them into original_translations/ in the root folder manually
# Listing file names
file_names <- list.files(here("original_translations/"), full.names = FALSE)
files      <- list.files(here("original_translations/"), full.names = TRUE)
# Extracting language names
languages <- gsub(" Translations - Transparency Checklist.xlsx", "", file_names)

# Prepearing the English version
## Since English was the original language of the app we do not have a translation for it
## Thus we extract the text from a random translation document
translation_df <- 
  readxl::read_excel(files[1], sheet = 2) %>% 
  as_tibble() %>% 
  dplyr::rename(English = `in English`) %>% 
  dplyr::select(English)

translation_df[1, "English"] <- "English"

# Reading the files iteratively
for(i in seq_along(files)){
  # Read the translation document
  sheet <- readxl::read_excel(files[i], sheet = 2)
  
  # Check if the number of rows are equal to the English
  if(nrow(sheet) != nrow(translation_df)) {
    message("The number of rows in the translation ", languages[i], " is not equal.")
    sheet <- readxl::read_excel(files[i], sheet = "EN1 to Your language")
  }
  
  # Add the new language to the translation table
  translation_df[[languages[i]]] <- sheet[, 2, drop=TRUE]
  
  # Use language name from translation file name if language name not provided
  # in the translation text
  if(is.na(translation_df[[languages[i]]][1])){
    message("For ", languages[i], " language name is not provided.")
    translation_df[[languages[i]]][1] <- languages[i]
  }
  
  # Delete "[]" from the language name if provided in the document
  translation_df[[languages[i]]][1] <- gsub("\\[", "", translation_df[[languages[i]]][1])
  translation_df[[languages[i]]][1] <- gsub("\\]", "", translation_df[[languages[i]]][1])
}

# Transform into a list
translations <- df2list(translation_df)

# Save translations
jsonlite::write_json(translations, here("./inst/app/www/translations.json"), pretty=TRUE)

