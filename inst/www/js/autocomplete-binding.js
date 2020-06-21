var autocompleteBinding = new Shiny.InputBinding();

$.extend(autocompleteBinding, {
  find: function (scope) {
    return $(scope).find(".autocomplete").find("input");
  },

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
    console.time("setValue");
    var arr = $(el).data("options"),
      labeled = !arr.length,
      upd = $(el).data("create");
    if (upd || (!labeled && arr.indexOf(value) > -1)) {
      $(el).attr("result", value);
      el.value = value;
    } else if (labeled && arr[value] !== undefined) {
      $(el).attr("result", arr[value]);
      el.value = value;
    }
    console.timeEnd("setValue");
  },

  subscribe: function (el, callback) {
    var setVal = this.setValue;
    $(el).on("input.autocompleteBinding", function (e) {
      setVal(el, el.value);
      callback(true);
    });
    $(el).on("focus.autocompleteBinding", function (e) {
      el.select();
    });
    $(el).on("change.autocompleteBinding", function (e) {
      setVal(el, el.value);
      callback(false);
    });
  },

  unsubscribe: function (el) {
    $(el).off(".autocompleteBinding");
  },

  receiveMessage: function (el, data) {
    if (data.hasOwnProperty("value")) this.setValue(el, data.value);
    if (data.hasOwnProperty("placeholder")) el.placeholder = data.placeholder;
    if (data.hasOwnProperty("label"))
      $(el)
        .parent()
        .find('label[for="' + el.id + '"]')
        .text(data.label);

    setData(data, "options", el);
    setData(data, "max", el);
    setData(data, "hide", el);
    setData(data, "create", el);
    setData(data, "contains", el);

    $(el).trigger("change");
  },

  getRatePolicy: function () {
    return { policy: "debounce", delay: 500 };
  },
});

Shiny.inputBindings.register(autocompleteBinding, "shiny.autocomplete");

function autocomplete(inp) {
  var currentFocus;

  $(inp).on("input.autocompleteBinding", function (e) {
    var $el = $(this);
    var arr = $el.data("options"),
      maxCount = $el.data("max"),
      hideValues = $el.data("hide"),
      contains = $el.data("contains"),
      val = this.value;

    closeAllLists();
    if (!val) return false;
    currentFocus = -1;

    var a = document.createElement("DIV");
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
      labeled = !arr.length;

    var onClick = function (ce) {
      $el.val($(ce.target).data("value")).trigger("change");
    };

    for (var i = 0; i < len; i++) {
      var lab, id;
      if (labeled) {
        lab = keys[i];
        id = arr[lab];
      } else {
        lab = id = arr[i];
      }
      var labUC = lab.toUpperCase();
      var pos = -1;
      if (contains) pos = labUC.indexOf(valUC);
      else if (labUC.substr(0, valLen) === valUC) pos = 0;
      if (pos >= 0) {
        if (valLen === lab.length) {
          closeAllLists();
          break;
        }
        var b = document.createElement("DIV");
        b.innerHTML = lab.substr(0, pos);
        b.innerHTML += "<strong>" + lab.substr(pos, valLen) + "</strong>";
        b.innerHTML += lab.substr(pos + valLen);
        if (labeled && !hideValues) b.innerHTML += "<small>" + id + "</small>";
        $(b).data("value", lab);
        $(b).on("click", onClick);
        a.appendChild(b);
        if (maxCount && ++count >= maxCount) break;
      }
    }
  });
  $(inp).on("keydown.autocompleteBinding", function (e) {
    var x,
      parent = document.getElementById(this.id + "autocomplete-list");
    if (parent) x = parent.getElementsByTagName("div");
    if (e.keyCode === 40) {
      //arrow DOWN
      currentFocus++;
      addActive(x);
    } else if (e.keyCode === 38) {
      //arrow UP
      currentFocus--;
      addActive(x);
    } else if (e.keyCode === 13) {
      //ENTER key
      e.preventDefault();
      if (currentFocus > -1 && x) {
        x[currentFocus].click();
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
}

function setData(obj, field, el) {
  if (obj.hasOwnProperty(field)) $(el).data(field, obj[field]);
}

function closeAllLists(elmnt) {
  var items = document.getElementsByClassName("autocomplete-items");
  for (var i = 0; i < items.length; i++) {
    if (elmnt != items[i]) items[i].parentNode.removeChild(items[i]);
  }
}

document.addEventListener("click", closeAllLists);
