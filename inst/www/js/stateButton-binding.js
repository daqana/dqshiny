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
    var $el = $(el), that = this;

    $el.on("click.stateButtonBinding", function(e) {
      let state = ($el.data("state") || 1) - 1;
      const states = $el.data("states");
      state = (state + 1) % states.length;
      that.setValue(el, state);
      $el.data("state", state + 1);
      callback();
    });

    $el.on("change.stateButtonBinding", function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off(".stateButtonBinding");
  },

  // Receive messages from the server.
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty("state")) $(el).data("state", data.state);
    if (data.hasOwnProperty("states")) $(el).data("states", data.states);
    this.setValue(el, $(el).data("state") - 1);
    $(el).trigger("change");
  },

  // This returns a full description of the input's state.
  getState: function(el) {
    return {
      value: this.getValue(el)
    };
  }

});

Shiny.inputBindings.register(stateButtonBinding, "shiny.stateButton");
