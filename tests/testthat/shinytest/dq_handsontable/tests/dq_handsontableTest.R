app <- ShinyDriver$new("../", cleanLogs = FALSE)
app$snapshotInit("dq_handsontableTest")

app$waitFor("loaded === true")
app$snapshot(screenshot = FALSE)
app$setInputs("randomTable-filter-B" = "f",
              "randomTable-pageSize" = "5")
app$snapshot(screenshot = FALSE)
app$setInputs("randomTable-filter-D" = "564")
app$snapshot(screenshot = FALSE)
app$setInputs("randomTable-filter-B" = "",
              "randomTable-filter-D" = "",
              "randomTable-pageSize" = "25",
              "randomTable-pageNum" = 3,
              "randomTable-filter-A" = "world!")
app$snapshot(screenshot = FALSE)
