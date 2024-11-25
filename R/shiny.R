shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  with(val, data.frame(
    id = unlist(ids), 
    reviewed = ifelse(review, "Yes", "No"),
    row_index = row
    ))
}, TRUE)
