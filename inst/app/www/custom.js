function ts(cb) {
  if (cb.readOnly) cb.checked=cb.readOnly=false;
  else if (!cb.checked) cb.readOnly=cb.indeterminate=true;
}

var customCheckbox = new Shiny.InputBinding();

$.extend(customCheckbox, {
  find: function(scope) {
    return $(scope).find("input[type='checkbox'].cs_checkbox");
  },
  getValue: function(el) {
    return el.checked;
  },
  subscribe: function(el, callback) {
    $(el).on("change.checkboxInputBinding", function() {
      Shiny.onInputChange($(this).attr('id'), this.checked, {priority: 'event'});
    });
  },
  unsubscribe: function(el) {
    $(el).off(".checkboxInputBinding");
  }
});

Shiny.inputBindings.register(customCheckbox);
