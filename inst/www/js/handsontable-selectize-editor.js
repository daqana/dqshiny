/// selectize plugin
(function(Handsontable) {
  "use strict";

  var SelectizeEditor = Handsontable.editors.TextEditor.prototype.extend();

  SelectizeEditor.prototype.prepare = function(row, col, prop, td, originalValue, cellProperties) {
    Handsontable.editors.TextEditor.prototype.prepare.apply(this, arguments);

    this.options = {};
    this.oldCaretPos = 1;

    if (this.cellProperties.selectizeOptions) {
      this.options = $.extend(this.options, cellProperties.selectizeOptions);
    }

    cellProperties.selectizeOptions = $.extend(
      {}, cellProperties.selectizeOptions
    );
  };

  SelectizeEditor.prototype.createElements = function() {
    this.$body = $(document.body);

    this.TEXTAREA = document.createElement("select");
    this.$select = $(this.TEXTAREA);

    Handsontable.dom.addClass(this.TEXTAREA, "handsontableInput");

    this.textareaStyle = this.TEXTAREA.style;
    this.textareaStyle.width = 0;
    this.textareaStyle.height = 0;

    this.TEXTAREA_PARENT = document.createElement("DIV");
    Handsontable.dom.addClass(this.TEXTAREA_PARENT, "handsontableInputHolder");

    this.textareaParentStyle = this.TEXTAREA_PARENT.style;
    this.textareaParentStyle.top = 0;
    this.textareaParentStyle.left = 0;
    this.textareaParentStyle.display = "none";

    this.TEXTAREA_PARENT.appendChild(this.TEXTAREA);

    this.instance.rootElement.appendChild(this.TEXTAREA_PARENT);

    var that = this;
    this.instance._registerTimeout(
      setTimeout(function() {
        that.refreshDimensions();
      }, 0)
    );
  };

  var onSelectizeChanged = function(value, $item) {
    var max = this.selectize.settings.maxItems;
    if (max && this.selectize.items.length >= max) {
      this.close();
      this.finishEditing();
    }
  };

  var onOptionAdded = function(value, data) {
    this.selectize.setValue(value);
  };

  var onBeforeKeyDown = function(event) {
    var editor = this.getActiveEditor();
    var keyCodes = Handsontable.helper.KEY_CODES;
    // catch CTRL but not right ALT (which in some systems triggers ALT+CTRL)
    var ctrlDown = (event.ctrlKey || event.metaKey) && !event.altKey;
    var sel = editor.selectize;
    var multiple = editor && sel.settings.mode == "multi";

    // Process only events that have been fired in the editor
    if (event.target.tagName !== "INPUT") {
      // except BACKSPACEs for deleting the last item since there won't be an INPUT if selectize is full
      var vals = sel.getValue();
      if (event.keyCode === keyCodes.BACKSPACE && vals.length) {
        sel.removeItem(vals[vals.length - 1]);
        sel.open();
        event.stopImmediatePropagation();
      }
      return;
    }
    if (event.keyCode === 17 || event.keyCode === 224 || event.keyCode === 91 || event.keyCode === 93) {
      // when CTRL or its equivalent is pressed and cell is edited, don't prepare selectable text in textarea
      event.stopImmediatePropagation();
      return;
    }

    switch (event.keyCode) {
      case keyCodes.ENTER:
        if (multiple && sel.settings.maxItems > sel.items.length) {
          event.stopImmediatePropagation();
        }
        break;

      case keyCodes.ARROW_UP:
      case keyCodes.ARROW_DOWN:
      case keyCodes.BACKSPACE:
      case keyCodes.HOME:
      case keyCodes.END:
        event.stopImmediatePropagation();
        break;

      case keyCodes.ARROW_RIGHT:
        if (!multiple || editor.oldCaretPos < sel.items.length) {
          event.stopImmediatePropagation();
        }
        editor.oldCaretPos = sel.caretPos;
        break;

      case keyCodes.ARROW_LEFT:
        if (!multiple || editor.oldCaretPos > 0) {
          event.stopImmediatePropagation();
        }
        editor.oldCaretPos = sel.caretPos;
        break;

      case keyCodes.A:
      case keyCodes.X:
      case keyCodes.C:
      case keyCodes.V:
        if (ctrlDown) {
          event.stopImmediatePropagation();
          //CTRL+A, CTRL+C, CTRL+V, CTRL+X should only work locally when cell is edited (not in table context)
        }
        break;
    }
  };

  SelectizeEditor.prototype.open = function(keyboardEvent) {
    this.refreshDimensions();
    this.textareaParentStyle.display = "block";
    this.instance.addHook("beforeKeyDown", onBeforeKeyDown);

    this.$select.css({
      height: $(this.TD).height() + 4,
      "min-width": $(this.TD).outerWidth() - 4
    });

    var options = $.extend({}, this.options, {
      width: "100%",
      dropdownParent: "body"
    });

    if (options.maxItems === null || options.maxItems > 1) {
      options.plugins = ["remove_button"];
    }

    var originalValue = (this.originalValue + "").split(",");

    if (this.selectize) {
      this.selectize.destroy();
      // reset SELECT if several columns use this editor and some of them have maxItems > 1
      this.$select.attr("multiple", false);
    }

    options.items = originalValue;
    if (options.create && originalValue.toString().trim() !== "") {
      for (var i = 0; i < originalValue.length; i++) {
        options.options.push({
          value: originalValue[i],
          text: originalValue[i]
        });
      }
    }

    var $select = this.$select.selectize(options);
    this.selectize = $select[0].selectize;

    this.selectize.focus();
    this.selectize.on("item_add", onSelectizeChanged.bind(this));
    this.selectize.on("option_add", onOptionAdded.bind(this));
  };

  SelectizeEditor.prototype.init = function() {
    Handsontable.editors.TextEditor.prototype.init.apply(this, arguments);
  };

  SelectizeEditor.prototype.close = function() {
    this.instance.listen();
    this.instance.removeHook("beforeKeyDown", onBeforeKeyDown);
    Handsontable.editors.TextEditor.prototype.close.apply(this, arguments);
  };

  SelectizeEditor.prototype.getValue = function() {
    if (!this.selectize) {
      return "";
    }
    var vals = this.selectize.getValue();
    if (typeof vals == "string") {
      return vals;
    }
    return vals.join(",");
  };

  SelectizeEditor.prototype.focus = function() {
    this.instance.listen();

    // DO NOT CALL THE BASE TEXTEDITOR FOCUS METHOD HERE, IT CAN MAKE THIS EDITOR BEHAVE POORLY AND HAS NO PURPOSE WITHIN THE CONTEXT OF THIS EDITOR
    //Handsontable.editors.TextEditor.prototype.focus.apply(this, arguments);
  };

  SelectizeEditor.prototype.beginEditing = function(initialValue) {
    var onBeginEditing = this.instance.getSettings().onBeginEditing;
    if (onBeginEditing && onBeginEditing() === false) {
      return;
    }
    Handsontable.editors.TextEditor.prototype.beginEditing.apply(this, arguments);
  };

  SelectizeEditor.prototype.finishEditing = function(isCancelled, ctrlDown) {
    this.instance.listen();
    return Handsontable.editors.TextEditor.prototype.finishEditing.apply(this, arguments);
  };

  Handsontable.editors.SelectizeEditor = SelectizeEditor;
  Handsontable.editors.registerEditor("selectize", SelectizeEditor);
})(Handsontable);
