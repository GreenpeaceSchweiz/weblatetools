editTranslationFile <-
  function(slug, from.language, filter, replace) {
    df <- readFile(slug, from.language)
    if (!missing(filter)) {
      df <-
        subset(df, !grepl(paste(filter, collapse = "|"), target, ignore.case = TRUE))
    }
    if (!missing(replace)) {
      df$target <- gsub(replace$pattern, replace$replace, df$target)
    }
    writeToFile(df, filename = slug, from.language)
  }
