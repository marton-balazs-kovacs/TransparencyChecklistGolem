# Long checklist
questions <- jsonlite::read_json(path = "inst/app/www/questions.json")

source("R/assign_id.R")

# Get UI elements for long checklist
long <- rlang::list2(
  headList = questions$Head,
  sectionsList = assign_id(questions$Sections),
  answerList = questions$Answers
)

# Save lists as internal data
usethis::use_data(long, overwrite = TRUE, internal = TRUE)

# Short checklist
short_questions <- jsonlite::read_json(path = "inst/app/www/questions.json")

# usethis::use_data(questions, overwrite = TRUE)
