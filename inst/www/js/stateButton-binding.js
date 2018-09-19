var stateButtonBinding = new Shiny.InputBinding();

$.extend(stateButtonBinding, {
  find: function(scope) {
    return $(scope).find(".state-button");
  },

  // return the ID of the DOM element
  getId: function(el) {
    return el.id;
  },

  initialize: function(el) {},

  getValue: function(el) {
    return $(el).data("state");
  },

  setValue: function(el, value) {
    $(el).find("i")[0].className = "fa fa-" + $(el).data("states")[value];
  },

  subscribe: function(el, callback) {
    var $el = $(el),
      that = this;
    var currentState = ($el.data("state") || 1) - 1;

    $el.on("click.stateButtonBinding", function(e) {
      var states = $el.data("states");
      currentState = (currentState + 1) % states.length;
      that.setValue(el, currentState);
      $el.data("state", currentState + 1);
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".stateButtonBinding");
  },

  // Receive messages from the server.
  // Messages sent by updateUrlInput() are received by this function.
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty("state")) $(el).data("state", data.state);
    if (data.hasOwnProperty("states")) $(el).data("states", data.states);
    this.setValue(el, $(el).data("state") - 1);
    $(el).trigger("change");
  },

  // This returns a full description of the input's state.
  // Note that some inputs may be too complex for a full description of the
  // state to be feasible.
  getState: function(el) {
    return {
      label: "test",
      value: ""
    };
  },

  // The input rate limiting policy
  getRatePolicy: function() {
    return {
      // Can be 'debounce' or 'throttle'
      policy: "debounce",
      delay: 500
    };
  }
});

Shiny.inputBindings.register(stateButtonBinding, "shiny.stateButton");
