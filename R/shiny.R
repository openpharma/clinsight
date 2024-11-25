shiny::registerInputHandler('CS.reviewInfo', function(val, ...) {
  with(val, data.frame(
    id = unlist(ids), 
    reviewed = ifelse(review, "Yes", "No"),
    row_index = row
    ))
}, TRUE)

checkbox_callback <- DT::JS(
  "table.on('click', 'input[type=\"checkbox\"]', function(){",
  "var tblId = $(this).closest('.datatables').attr('id');",
  "var cell = table.cell($(this).closest('td'));",
  "var rowIdx = table.row($(this).closest('tr')).index();",
  "var ids = cell.data().ids;",
  "var review = $(this).is(':checked');",
  "var info = {review: review, ids: ids, row: tblId + '_row_' + rowIdx};",
  "Shiny.setInputValue(tblId + '_review_selection:CS.reviewInfo', info);",
  "})"
)

checkbox_render <- DT::JS(
  "function(data, type, row, meta) {",
  "var reviewed = data.reviewed;",
  "return `<input type='checkbox' class='${reviewed == null ? 'indeterminate' : reviewed ? 'checked' : 'unchecked'}' ${reviewed ? 'checked' : ''} ${reviewed == null ? 'onclick=\"ts(this)\"' : ''}/>`;",
  "}"
)

checkbox_create_callback <- DT::JS(
  "function(row, data, dataIndex) {",
  "if (data[0] == null) {",
  "let cb = row.cells[0].getElementsByTagName('input')[0]",
  "cb.indeterminate = cb.readOnly = true;",
  "}",
  "}"
)
