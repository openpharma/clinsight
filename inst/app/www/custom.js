function ts(cb) {
  if (cb.readOnly) {
    cb.indeterminate=true;
    cb.readOnly=cb.checked=false;
  } else if (!cb.checked) {
    cb.readOnly=true;
    cb.indeterminate=false;
  }
}

var customCheckbox = new Shiny.InputBinding();

$.extend(customCheckbox, {
  find: function(scope) {
    return $(scope).find("input[type='checkbox'].cs_checkbox");
  },
  getValue: function(el) {
    return el.checked;
  },
  setValue: function(el, value) {
    el.checked = value;
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
