# TODO: "All Authors [Given name(s) Surname;]" not translated if used as a key I guess it is because special characters are used
# have to find a robust way for taking out special characters from keys  and referencing them in the app.

library(rjson)
library(purrr)
library(stringr)
library(tibble)
library(rlang)
library(jsonlite)

translations <- rjson::fromJSON(file = "./inst/app/www/translations.json")
language_codes <- rjson::fromJSON(file = "./inst/app/www/language_codes.json")

names(language_codes)
names(translations)

translations[["languages"]] <- NULL

language_list <- language_codes %>%
  rlang::set_names(purrr::pluck(translations, 1, 1))

language_list_json <- jsonlite::toJSON(language_list, pretty = TRUE, auto_unbox = TRUE)

write(language_list_json, "inst/app/www/language_list.json")

translations <- purrr::flatten(translations)

i18n_codes <- purrr::map(translations, "English")
i18n_codes

transpose_translations <- 
  purrr::transpose(translations) %>% 
  purrr::map(set_names, nm = i18n_codes) %>%
  # This should be done by match
  rlang::set_names(language_codes) %>% 
  map(
    ~ list(.) %>% 
      set_names("translation")
    )

# i18n_translations <- rjson::toJSON(transpose_translations, indent = 1)
i18n_translations <- jsonlite::toJSON(transpose_translations, pretty = TRUE, auto_unbox = TRUE)

write(i18n_translations, "inst/app/www/i18n_locales.json")

# usethis::use_data(transform_json, overwrite = TRUE)
