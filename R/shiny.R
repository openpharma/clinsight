shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  with(val, data.frame(
    id = unlist(ids), 
    reviewed = ifelse(isTRUE(review), "Yes", ifelse(isFALSE(review), "No", NA_character_))
    ))
}, TRUE)

checkbox_callback <- DT::JS(
  "table.on('click', 'input[type=\"checkbox\"]', function(){",
  "var tblId = $(this).closest('.datatables').attr('id');",
  "var cell = table.cell($(this).closest('td'));",
  "var rowIdx = table.row($(this).closest('tr')).index();",
  "var ids = cell.data().ids;",
  "var review = $(this).is(':indeterminate') ? null : $(this).is(':checked');",
  "cell.data().updated = review;",
  "var info = {review: review, ids: ids, row: tblId + '_row_' + rowIdx};",
  "Shiny.setInputValue(tblId + '_review_selection:CS.reviewInfo', info);",
  "})"
)

checkbox_render <- DT::JS(
  "function(data, type, row, meta) {",
  "var reviewed = data.reviewed;",
  "var updated = data.updated;",
  "return `<input type='checkbox' ",
    "class='${reviewed == null ? 'indeterminate' : reviewed ? 'checked' : 'unchecked'}' ",
    "${updated == null ? (reviewed ? 'checked' : '') : (updated ? 'checked' : '')} ",
    "${reviewed == null ? 'onclick=\"ts(this)\"' : ''}/>`;",
  "}"
)

checkbox_create_callback <- DT::JS(
  "function(row, data, dataIndex) {",
  "if (data[0].reviewed == null) {",
  "let cb = row.cells[0].getElementsByTagName('input')[0]",
  "cb.indeterminate = true;",
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
