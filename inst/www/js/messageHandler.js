function toggleState(params) {
  var objects = _getElement(params.id), $obj;
  for (var i = 0; i < objects.length; i++) {
    $obj = objects[i];
    if ($obj !== null) {
      if ($obj.hasClass("selectized")) {
        toggleSelectizeState(params, $obj[0]);
      } else if ($obj.hasClass("js-range-slider")) {
        toggleSliderState(params, $obj.data("ionRangeSlider"));
      } else {
        if ($obj.hasClass("shiny-date-input")) {
          $obj = $obj.find("input");
        }
        if (params.state !== null && typeof (params.state) != "undefined") {
          $obj.prop('disabled', params.state);
        } else {
          $obj.prop('disabled', function (i, v) { return !v; });
        }
      }
    }
  }

}
Shiny.addCustomMessageHandler("toggleState", toggleState);

function toggleClass(params) {
  var objects = _getElement(params.id), $obj;
  for (var i = 0; i < objects.length; i++) {
    $obj = objects[i];
    if ($obj !== null) {
      if (params.state !== null && typeof (params.state) != "undefined") {
        $obj.toggleClass(params.class_name, params.state);
      } else {
        $obj.toggleClass(params.class_name);
      }
    }
  }
}
Shiny.addCustomMessageHandler("toggleClass", toggleClass);

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

//######################## Helper ################################

function _getElement(id) {
  var ids, tmp, objects = [];
  if (typeof id == "string") {
    ids = [id];
  } else {
    ids = id;
  }
  for (var i = 0; i < ids.length; i++) {
    tmp = ids[i];
    var obj = document.getElementById(tmp);
    if (obj === null) {
      if (!tmp.startsWith("#")) {
        tmp = "#" + tmp;
      }
      $obj = $(tmp);
    } else {
      $obj = $(obj);
    }
    if ($obj.length === 0) {
      $obj = null;
    }
    objects.push($obj);
  }
  return objects;
}

function toggleSelectizeState(params, $obj) {
  if (params.state !== null && typeof (params.state) != "undefined") {
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
  if (params.state !== null && typeof (params.state) != "undefined") {
    $slider.update({ disable: params.state });
  } else {
    $slider.update({ disable: !$slider.options.disable });
  }
}
