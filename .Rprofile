source("renv/activate.R")

setHook("before.knit", function() {
  tryCatch(styler::style_file(rstudioapi::getActiveDocumentContext()$path),
           error = function(e) NULL)
}, action = "append")
