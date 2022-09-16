#' Edit translation files
#'
#' Takes one translation file from disk, removes unwanted translations, edits translations according to provided patterns, splits the file into multiple if too big, saves to disk, and returns the number of created files.
#'
#' @param slug The slug of the component the translation file belongs to.
#' @param from.language The source language of the translation file.
#' @param filter Optional. List of strings. Translations containing any of these strings will be removed from the translation file.
#' @param replace Optional. Data frame of search/replace string pairs. Needs to contain a column "pattern" and a column "replace". Translation strings will be edited according to these patterns.
#' @param size.limit The servers file size limit for uploads. Defaults to 800000. Decrease if getting HTTP 413 error on writing. Translation files bigger than this will be split into multiple smaller files.
#'
#' @return The number of saved processed files.
#' @export
editTranslationFile <-
  function(slug,
           from.language,
           filter,
           replace,
           size.limit = 800000) {
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
