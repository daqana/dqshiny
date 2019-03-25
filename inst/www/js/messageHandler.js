function dqToggleState(params) {
  var $obj = dqGetElement(params.id);
  if ($obj !== null) {
    if ($obj.hasClass("selectized")) {
      dqToggleSelectizeState(params, $obj[0]);
    } else if ($obj.hasClass("js-range-slider")) {
      dqToggleSliderState(params, $obj.data("ionRangeSlider"));
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
        dqToggleState(params);
      }, 1000);
    }
  }
}
Shiny.addCustomMessageHandler("dqToggleState", dqToggleState);

function dqToggleClass(params) {
  var $obj = dqGetElement(params.id);
  if ($obj !== null) {
    if ($obj.hasClass("selectized") || $obj.hasClass("js-range-slider")) {
      $obj = $obj.closest(".form-group");
    }
    if (params.state !== null && typeof params.state != "undefined") {
      $obj.toggleClass(params.className, params.state);
    } else {
      $obj.toggleClass(params.className);
    }
  } else if (params.retry) {
    params.retry = false;
    setTimeout(function() {
      dqToggleClass(params);
    }, 1000);
  }
}
Shiny.addCustomMessageHandler("dqToggleClass", dqToggleClass);

function dqScrollToElement(params) {
  var element = params,
    highlight = false;
  if (typeof params != "string") {
    element = params.id;
    highlight = params.highlight;
  }
  var $el = dqGetElement(element),
    pos,
    $parentEl = $el;
  while ($parentEl !== null && (pos = $parentEl.offset().top) === 0) {
    $parentEl = $parentEl.parent();
  }
  $("html, body").animate({ scrollTop: pos - $(window).height() / 3 }, 500);
  if (highlight) {
    setTimeout(function() {
      dqHighlight($el);
    }, 500);
  }
}
Shiny.addCustomMessageHandler("dqScrollToElement", dqScrollToElement);

function dqHighlightElement(params) {
  var element = params;
  if (typeof params != "string") {
    element = params.id;
  }
  var $el = dqGetElement(element);
  dqHighlight($el);
}
Shiny.addCustomMessageHandler("dqHighlightElement", dqHighlightElement);

function dqClickElement(params) {
  var element = params;
  if (typeof params != "string") {
    element = params.id;
  }
  var $el = dqGetElement(element);
  $el[0].click();
}
Shiny.addCustomMessageHandler("dqClickElement", dqClickElement);

function dqSelectTableCell(params) {
  dqScrollToElement("#" + params[0]);
  var $table = HTMLWidgets.getInstance(document.getElementById(params[0])).hot;
  $table.selectCell(parseInt(params[1]), parseInt(params[2]));
}
Shiny.addCustomMessageHandler("dqSelectTableCell", dqSelectTableCell);

function dqResetSlider(params) {
  var $obj = dqGetElement(params.id);
  if ($obj !== null && $obj.data("ionRangeSlider") !== null) {
    $obj.data("ionRangeSlider").reset();
  }
}
Shiny.addCustomMessageHandler("dqResetSlider", dqResetSlider);

function dqRenderHot(params) {
  HTMLWidgets.getInstance(document.getElementById(params.id)).hot.render();
}
Shiny.addCustomMessageHandler("dqRenderHot", dqRenderHot);

function dqUpdateBox(params) {
  var $obj = dqGetElement(params.id);
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
  } else if (params.retry) {
    params.retry = false;
    setTimeout(function() {
      dqUpdateBox(params);
    }, 500);
  }
}
Shiny.addCustomMessageHandler("dqUpdateBox", dqUpdateBox);

//######################## Helper ################################

function dqGetElement(id) {
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

function dqToggleSelectizeState(params, $obj) {
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

function dqToggleSliderState(params, $slider) {
  if (params.state !== null && typeof params.state != "undefined") {
    $slider.update({ disable: params.state });
  } else {
    $slider.update({ disable: !$slider.options.disable });
  }
}

function dqHighlight($el) {
  if ($el.css("display") == "none" || $el.hasClass("js-range-slider")) {
    var $parEl = $el.closest(".form-group");
    if ($parEl.length) $el = $parEl;
  }
  $el.addClass("dq-highlighted");
  $el.on("animationend", function() {
    $el.removeClass("dq-highlighted");
  });
}
