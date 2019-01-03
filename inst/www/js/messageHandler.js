function toggleState(params) {
  var $obj = _getElement(params.id);
  if ($obj !== null) {
    if ($obj.hasClass("selectized")) {
      toggleSelectizeState(params, $obj[0]);
    } else if ($obj.hasClass("js-range-slider")) {
      toggleSliderState(params, $obj.data("ionRangeSlider"));
    } else {
      if ($obj.hasClass("shiny-date-input")) {
        $obj = $obj.find("input");
      }
      if (params.state !== null && typeof params.state != "undefined") {
        $obj.prop("disabled", params.state);
      } else {
        $obj.prop("disabled", function(i, v) {
          return !v;
        });
      }
    }
  } else {
    if (params.retry) {
      params.retry = false;
      setTimeout(function() {
        toggleState(params);
      }, 1000);
    }
  }
}
Shiny.addCustomMessageHandler("toggleState", toggleState);

function toggleClass(params) {
  var $obj = _getElement(params.id);
  if ($obj !== null) {
    if ($obj.hasClass("selectized") || $obj.hasClass("js-range-slider")) {
      $obj = $obj.closest(".form-group");
    }
    if (params.state !== null && typeof params.state != "undefined") {
      $obj.toggleClass(params.className, params.state);
    } else {
      $obj.toggleClass(params.className);
    }
  } else {
    if (params.retry) {
      params.retry = false;
      setTimeout(function() {
        toggleClass(params);
      }, 1000);
    }
  }
}
Shiny.addCustomMessageHandler("toggleClass", toggleClass);

function scrollToElement(params) {
  var element = params,
    highlight = false;
  if (typeof params != "string") {
    element = params.id;
    highlight = params.highlight;
  }
  var $el = _getElement(element),
    pos,
    $parentEl = $el;
  while ($parentEl !== null && (pos = $parentEl.offset().top) === 0) {
    $parentEl = $parentEl.parent();
  }
  $("html, body").animate({ scrollTop: pos - $(window).height() / 3 }, 500);
  if (highlight) {
    setTimeout(function() {
      _highlight($el);
    }, 500);
  }
}
Shiny.addCustomMessageHandler("scrollToElement", scrollToElement);

function highlightElement(params) {
  var element = params;
  if (typeof params != "string") {
    element = params.id;
  }
  var $el = _getElement(element);
  _highlight($el);
}
Shiny.addCustomMessageHandler("highlightElement", highlightElement);

function clickElement(params) {
  var element = params;
  if (typeof params != "string") {
    element = params.id;
  }
  var $el = _getElement(element);
  $el[0].click();
}
Shiny.addCustomMessageHandler("clickElement", clickElement);

function selectTableCell(params) {
  scrollToElement("#" + params[0]);
  var $table = HTMLWidgets.getInstance(document.getElementById(params[0])).hot;
  $table.selectCell(parseInt(params[1]), parseInt(params[2]));
}
Shiny.addCustomMessageHandler("selectTableCell", selectTableCell);

function resetSlider(params) {
  var $obj = _getElement(params.id);
  if ($obj !== null && $obj.data("ionRangeSlider") !== null) {
    $obj.data("ionRangeSlider").reset();
  }
}
Shiny.addCustomMessageHandler("resetSlider", resetSlider);

function renderHot(params) {
  HTMLWidgets.getInstance(document.getElementById(params.id)).hot.render();
}
Shiny.addCustomMessageHandler("renderHot", renderHot);

function updateBox(params) {
  var $obj = _getElement(params.id);
  if ($obj !== null) {
    var open = $obj.find(".collapse").hasClass("in");
    var coll = params.collapsed;
    if ((coll === true && open) || (coll === false && !open) || coll === null) {
      if (params.silent) {
        $obj.find(".collapse").toggleClass("in");
      } else {
        $obj.find(".collapser").click();
      }
    }
  }
}
Shiny.addCustomMessageHandler("updateBox", updateBox);

//######################## Helper ################################

function _getElement(id) {
  var obj = document.getElementById(id);
  if (obj === null) {
    if (!id.startsWith("#")) {
      id = "#" + id;
    }
    $obj = $(id);
  } else {
    $obj = $(obj);
  }
  if ($obj.length === 0) {
    $obj = null;
  }
  return $obj;
}

function toggleSelectizeState(params, $obj) {
  if (params.state !== null && typeof params.state != "undefined") {
    if (params.state) {
      $obj.selectize.disable();
    } else {
      $obj.selectize.enable();
    }
  } else {
    if ($obj.selectize.isDisabled) {
      $obj.selectize.enable();
    } else {
      $obj.selectize.disable();
    }
  }
}

function toggleSliderState(params, $slider) {
  if (params.state !== null && typeof params.state != "undefined") {
    $slider.update({ disable: params.state });
  } else {
    $slider.update({ disable: !$slider.options.disable });
  }
}

function _highlight($el) {
  if ($el.css("display") == "none" || $el.hasClass("js-range-slider")) {
    var $parEl = $el.closest(".form-group");
    if ($parEl.length) $el = $parEl;
  }
  $el.addClass("dq-highlighted");
  $el.on("animationend", function() {
    $el.removeClass("dq-highlighted");
  });
}
