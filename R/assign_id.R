#' Assign id to widgets
#' 
#' Name the questions (ind_1 ... ind_n) - this slightly reduces the
#' tedious filling in of question numbers in .json
#' and reduces the likelihood of a manual mistake.
assign_id <- function(sections) {
  ind <- 1
  sectionsList <- lapply(sections, function(Sec){
    Sec$Questions <- lapply(Sec$Questions, function(x) {
      
      if(is.null(x$Name)){ # create names for questions in format ind_number of question
        x <- c(x, Name = paste0("ind_", ind))
        
        # add the number of the question to the question label
        # x$Label <- paste0("(", ind, ") ", x$Label)
        ind <<- ind + 1 
      }
      
      x
    })
    
    Sec$Value <- digest::digest(Sec$Name)
    
    Sec
  })
  
  return(sectionsList)
}
