shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  with(val, data.frame(
    id = unlist(ids), 
    reviewed = ifelse(isTRUE(review), "Yes", ifelse(isFALSE(review), "No", NA_character_)),
    row_id = row_id
    ))
}, TRUE)

checkbox_callback <- DT::JS(
  "table.on('column-reorder', function() {",
    "table.rows().every(function() {",
      "if (this.data()[0].reviewed == null) {",
        "$(':checkbox', this.node()).",
          "addClass('indeterminate').",
          "prop('indeterminate', this.data()[0].updated == null).",
          "prop('readOnly', this.data()[0].updated == false)",
      "}",
    "})",
  "});",
  "table.on('click', 'input[type=\"checkbox\"]', function(){",
    "var tblId = $(this).closest('.datatables').attr('id');",
    "var cell = table.cell($(this).closest('td'));",
    "var review = $(this).is(':indeterminate') ? null : $(this).is(':checked');",
    "cell.data().updated = review;",
    "var info = {review: review, ids: cell.data().ids, row_id: cell.data().row_id};",
    "Shiny.setInputValue(tblId + '_review_selection:CS.reviewInfo', info);",
  "});"
)

checkbox_render <- DT::JS(
  "function(data, type, row, meta) {",
    "var reviewed = data.reviewed;",
    "var updated = data.updated;",
    "var cb_class = ''",
    "if (reviewed == null) {",
      "cb_class = updated == null ? '' : 'indeterminate'",
    "} else {",
      "cb_class = reviewed ? 'checked' : 'unchecked'",
    "}",
    "return `<input type='checkbox' ",
      "class='${cb_class}' ",
      "${updated == null ? (reviewed ? 'checked' : '') : (updated ? 'checked' : '')} ",
      "${reviewed == null ? 'onclick=\"ts(this)\"' : ''}/>`;",
  "}"
)

row_callback <- DT::JS(
  "function(row, data) {",
    "if (data[0].reviewed == null) {",
      "$(':checkbox', row).",
        "addClass('indeterminate').",
        "prop('indeterminate', data[0].updated == null).",
        "prop('readOnly', data[0].updated == false)",
    "}",
  "}"
)

update_cbs <- function(id, checked, session = getDefaultReactiveDomain()) {
  tblId <- session$ns(id)
  params <- list(id = tblId, checked = checked)
  session$sendCustomMessage('update_checkboxes', params)
}

progress_bar <- function(outputId) {
  div(
    id = outputId,
    class = "cs-progress-container",
    div(
      class = "cs-progress-bar",
      div(class = c("cs-progress", "completed")),
      div(class = c("cs-progress", "unmarking")),
      div(class = c("cs-progress", "marking"))
    ),
    div(
      class = "cs-completed"
    )
  )
}

render_progress_bar <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- exprToFunction(expr, env, quoted)
  
  function(){
    func()
  }
}
