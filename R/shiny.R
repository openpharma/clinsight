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
  "var info = {review: review, ids: ids, row: tblId + '_row_' + rowIdx};",
  "Shiny.setInputValue(tblId + '_review_selection:CS.reviewInfo', info);",
  "})"
)

checkbox_render <- DT::JS(
  "function(data, type, row, meta) {",
  "var reviewed = data.reviewed;",
  "return `<input type='checkbox' ",
    "class='${reviewed == null ? 'indeterminate' : reviewed ? 'checked' : 'unchecked'}' ",
    "${reviewed ? 'checked' : ''} ",
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

update_cbs <- function(tblId, checked) {
  "$(':checkbox:not(.indeterminate)', $('#%s .table').DataTable().rows().nodes()).prop('checked', %s)" |> 
    sprintf(tblId, tolower(checked)) |> 
    shinyjs::runjs()
  "$(':checkbox.indeterminate', $('#%s .table').DataTable().rows().nodes()).prop('checked', %s).prop('indeterminate', false).prop('readOnly', %s)" |> 
    sprintf(tblId, tolower(checked), tolower(!checked)) |> 
    shinyjs::runjs()
}

progress_bar <- function(outputId) {
  div(
    id = outputId,
    class = "cs-progress-bar",
    div(class = c("cs-progress", "completed")),
    div(class = c("cs-progress", "unmarking")),
    div(class = c("cs-progress", "marking"))
  )
}

render_progress_bar <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- exprToFunction(expr, env, quoted)
  
  function(){
    func()
  }
}
