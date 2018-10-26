var timeInputBinding = new Shiny.InputBinding();

$.extend(timeInputBinding, {
  find: function (scope) {
    return $(scope)
      .find(".time-input")
      .find("input");
  },

  // return the ID of the DOM element
  getId: function (el) {
    return el.id;
  },

  initialize: function (el) {
    if ($(el).attr("ui") == "material") {
      const format = $(el).attr("format") || "HH:mm";
      const min = moment(el.min, format);
      const max = moment(el.max, format);
      $(el).bootstrapMaterialDatePicker({
				date: false,
				shortTime: false,
				format: format,
				minDate: (el.min && min) || null,
				maxDate: (el.max && max) || null,
				color: $(el).attr("color")
			});
    }
  },

  getValue: function (el) {
    if (el) return el.value;
    return "";
  },

  setValue: function (el, value) {
    el.value = value;
  },

  subscribe: function (el, callback) {
    var setVal = this.setValue;
    if ($(el).attr("ui") == "material") {
      $(el).on("change.timeInputBinding", function (event, date) {
        if (date) {
          setVal(el, date.format($(el).attr("format") || "HH:mm"));
        }
        callback(false);
      });
    } else {
      $(el).on("change.timeInputBinding", function (event, date) {
        callback(true);
      });
    }
  },

  unsubscribe: function (el) {
    $(el).off(".timeInputBinding");
  },

  // Receive messages from the server.
  receiveMessage: function (el, data) {
    if (data.hasOwnProperty("value")) el.value = data.value;
    if (data.hasOwnProperty("placeholder")) el.placeholder = data.placeholder;
    if (data.hasOwnProperty("label"))
      $(el).parent().find('label[for="' + el.id + '"]').text(data.label);

    if ($(el).attr("ui") == "material") {
      const format = $(el).attr("format");
      if (data.hasOwnProperty("min"))
        $(el).bootstrapMaterialDatePicker("setMinDate", moment(data.min, format));
      if (data.hasOwnProperty("max"))
        $(el).bootstrapMaterialDatePicker("setMaxDate", moment(data.max, format));
    }

    $(el).trigger("change");
  },

  // This returns a full description of the input's state.
  getState: function (el) {
    return {
      label: "test",
      value: ""
    };
  },

  // The input rate limiting policy
  getRatePolicy: function () {
    return {
      // Can be 'debounce' or 'throttle'
      policy: "debounce",
      delay: 500
    };
  }
});

Shiny.inputBindings.register(timeInputBinding, "shiny.timeInputBinding");
