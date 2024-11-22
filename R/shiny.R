shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  val[["ids"]] <- unlist(val[["ids"]])
  val
}, TRUE)