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
    if (el.type != "time") {
      $(el).bootstrapMaterialDatePicker({
				date: false,
				shortTime: false,
				format: "HH:mm"
			});
    }
  },

  getValue: function (el) {
    if (el) return $(el).attr("value");
    return "";
  },

  setValue: function (el, value) {
    el.value = value;
  },

  subscribe: function (el, callback) {
    var setVal = this.setValue;
    if (el.type != "time") {
      $(el).on("change.timeInputBinding", function (event, date) {
        if (date) {
          setVal(el, date.format("HH:mm"));
        }
        callback(false);
      });
    }
  },

  unsubscribe: function (el) {
    $(el).off(".timeInputBinding");
  },

  // Receive messages from the server.
  // Messages sent by updateUrlInput() are received by this function.
  receiveMessage: function (el, data) {
    if (data.hasOwnProperty("value")) el.value = data.value;
    if (data.hasOwnProperty("placeholder")) el.placeholder = data.placeholder;
    if (data.hasOwnProperty("label"))
      $(el).parent().find('label[for="' + el.id + '"]').text(data.label);

    $(el).trigger("change");
  },

  // This returns a full description of the input's state.
  // Note that some inputs may be too complex for a full description of the
  // state to be feasible.
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
