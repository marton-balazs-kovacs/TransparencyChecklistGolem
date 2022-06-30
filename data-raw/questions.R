# Long checklist
questions <- jsonlite::read_json(path = "inst/app/www/questions.json")

source("R/assign_id.R")

# Get UI elements for long checklist
long <- rlang::list2(
  headList = questions$Head,
  sectionsList = assign_id(questions$Sections),
  answerList = questions$Answers
)

# Short checklist
short_questions <- jsonlite::read_json(path = "inst/app/www/questionsShort.json")

# Get UI elements for short checklist
short <- rlang::list2(
  headList = short_questions$Head,
  sectionsList = assign_id(short_questions$Sections),
  answerList = short_questions$Answers
)

# Save lists as internal data
usethis::use_data(long, short, overwrite = TRUE, internal = TRUE)

# Get all the possible question dependencies for testing
depends <-
  map(long$sectionsList, ~flatten(.x["Questions"])) %>% 
  flatten(.) %>% 
  flatten(.) %>% 
  keep(.p = stringr::str_detect(names(.), "Depends"))
