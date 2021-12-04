editTranslationFile <-
  function(slug, from.language, filter, replace, size.limit) {
    df <- readFile(slug, from.language)

    # Filter
    if (!missing(filter)) {
      df <-
        subset(df, !grepl(paste(filter, collapse = "|"), target, ignore.case = TRUE))
    }

    # Replace
    if (!missing(replace)) {
      df$target <- gsub(replace$pattern, replace$replace, df$target)
    }

    # Split
    count <-  ceiling(file.size(filePath(from.language, slug)) / size.limit)
    seg.length <- floor(nrow(df) / count)
    for (i in 1:count) {
      output <- df[((i-1)*seg.length + 1):(i*seg.length),]
      writeToFile(output, processedFile(slug, i), from.language)
    }
    if (count > 1) cat(paste(">> editing -  Split file into",
                             count,
                             "parts\n", sep = " "))
    return(count)
  }
