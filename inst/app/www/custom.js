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

var customProgressBar = new Shiny.OutputBinding();

$.extend(customProgressBar, {
  find: function(scope) {
    return $(scope).find("div.cs-progress-bar");
  },
  renderValue: function(el, data) {
    let cmp_pct = (data.completed-data.unmarking)/data.total*100;
    let um_pct = data.unmarking/data.total*100;
    let m_pct = data.marking/data.total*100;
    $('#' + el.id + " .cs-progress.completed").width(cmp_pct.toFixed(2) + "%")
    $('#' + el.id + " .cs-progress.unmarking").width(um_pct.toFixed(2) + "%")
    $('#' + el.id + " .cs-progress.marking").width(m_pct.toFixed(2) + "%")
  }
});

Shiny.outputBindings.register(customProgressBar)
