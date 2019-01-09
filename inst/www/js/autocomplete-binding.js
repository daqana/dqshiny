var autocompleteBinding = new Shiny.InputBinding();

$.extend(autocompleteBinding, {
  find: function (scope) {
    return $(scope).find(".autocomplete").find("input");
  },

  // return the ID of the DOM element
  getId: function (el) {
    return el.id;
  },

  initialize: function (el) {
    if (el) autocomplete(el);
  },

  getValue: function (el) {
    if (el) return $(el).attr("result");
    return "";
  },

  setValue: function (el, value) {
    var arr = $(el).data("options"),
      keys = Object.keys(arr),
      labeled = !arr.length;
    if (!labeled && arr.indexOf(value) > -1) {
      $(el).attr("result", value);
      el.value = value;
    } else if (labeled && keys.indexOf(value) > -1) {
      $(el).attr("result", arr[value]);
      el.value = value;
    }
  },

  subscribe: function (el, callback) {
    var setVal = this.setValue;
    $(el).on("input.autocompleteBinding", function (event) {
      setVal(el, el.value);
      callback(true);
    });
    $(el).on("focus.autocompleteBinding", function (event) {
      el.select();
    });
    $(el).on("change.autocompleteBinding", function (event) {
      callback(false);
    });
    $("body").on("click.autocompleteBinding", ".auto_selector", function (event) {
      if (this.parentNode.id.startsWith(el.id)) {
        var val = this.getElementsByTagName("input")[0].value;
        setVal(el, val);
        callback(false);
      }
    });
  },

  unsubscribe: function (el) {
    $(el).off(".autocompleteBinding");
  },

  // Receive messages from the server.
  receiveMessage: function (el, data) {
    if (data.hasOwnProperty("value")) el.value = data.value;

    if (data.hasOwnProperty("label"))
      $(el).parent().find('label[for="' + el.id + '"]').text(data.label);

    if (data.hasOwnProperty("options")) $(el).data("options", data.options);
    if (data.hasOwnProperty("maxOptions")) $(el).data("max", data.maxOptions);
    if (data.hasOwnProperty("hideValues")) $(el).data("hide", data.hideValues);
    if (data.hasOwnProperty("placeholder")) el.placeholder = data.placeholder;

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

Shiny.inputBindings.register(autocompleteBinding, "shiny.autocomplete");

function autocomplete(inp) {
  var currentFocus;

  $(inp).on("input.autocompleteBinding", function (e) {
    $el = $(this);
    var a, b, i;
    var arr = $el.data("options"),
      maxCount = $el.data("max"),
      hideValues = $el.data("hide"),
      val = this.value;

    closeAllLists();
    if (!val) {
      return false;
    }
    currentFocus = -1;

    a = document.createElement("DIV");
    a.setAttribute("id", this.id + "autocomplete-list");
    a.setAttribute("class", "autocomplete-items");
    $(a).css("top", $el.offset().top + $el.outerHeight());
    $(a).css("left", $el.offset().left);
    $(a).width($el.innerWidth());
    document.body.appendChild(a);

    var valLen = val.length,
      valUC = val.toUpperCase(),
      keys = Object.keys(arr),
      len = keys.length,
      count = 0,
      labeled = !arr.length,
      lab,
      id;

    for (i = 0; i < len; i++) {
      if (labeled) {
        lab = keys[i];
        id = arr[keys[i]];
      } else {
        lab = id = arr[i];
      }

      if (lab.substr(0, valLen).toUpperCase() == valUC) {
        if (valLen == lab.length) {
          closeAllLists();
          break;
        }
        b = document.createElement("DIV");
        b.className = "auto_selector";
        b.innerHTML = "<strong>" + lab.substr(0, valLen) + "</strong>";
        b.innerHTML += lab.substr(valLen);
        b.innerHTML += "<input type='hidden' value='" + lab + "'>";
        if (labeled && !hideValues) {
          b.innerHTML += "<small>" + id + "</small>";
        }
        a.appendChild(b);
        if (maxCount && ++count >= maxCount) break;
      }
    }
  });
  $(inp).on("keydown.autocompleteBinding", function (e) {
    var x,
      parent = document.getElementById(this.id + "autocomplete-list");
    if (parent) x = parent.getElementsByTagName("div");
    if (e.keyCode == 40) {
      /*arrow DOWN*/
      currentFocus++;
      addActive(x);
    } else if (e.keyCode == 38) {
      /*arrow UP*/
      currentFocus--;
      addActive(x);
    } else if (e.keyCode == 13) {
      /*ENTER key*/
      e.preventDefault();
      if (currentFocus > -1) {
        if (x) x[currentFocus].click();
      }
    }
    if (x && x[currentFocus]) {
      var xot = x[currentFocus].offsetTop,
        xch = x[currentFocus].clientHeight,
        pst = parent.scrollTop,
        pch = parent.clientHeight;
      if (xot < pst) parent.scrollTop = xot;
      else if (xot + xch > pst + pch) parent.scrollTop = xot + xch - pch;
    }
  });
  function addActive(x) {
    if (!x) return false;
    removeActive(x);
    if (currentFocus >= x.length) currentFocus = 0;
    if (currentFocus < 0) currentFocus = x.length - 1;
    x[currentFocus].classList.add("autocomplete-active");
  }
  function removeActive(x) {
    for (var i = 0; i < x.length; i++) {
      x[i].classList.remove("autocomplete-active");
    }
  }
  function closeAllLists(elmnt) {
    var x = document.getElementsByClassName("autocomplete-items");
    for (var i = 0; i < x.length; i++) {
      if (elmnt != x[i] && elmnt != inp) {
        x[i].parentNode.removeChild(x[i]);
      }
    }
  }
  document.addEventListener("click", function (e) {
    closeAllLists(e.target);
  });
}
