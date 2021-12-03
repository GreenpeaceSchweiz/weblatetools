filePath <- function(language, fileName) {
  dir.create(file.path(language), showWarnings = FALSE)
  path <- file.path(language, paste(fileName, "csv", sep = "."))
  return(path)
}

readFile <- function(filename, language) {
  df <- readr::read_csv(filePath(language, filename),
                        show_col_types = FALSE,
                        lazy = FALSE) # to prevent locking of the file
  return(df)
}

writeToFile <- function(df, filename, language) {
  readr::write_csv(
    df,
    filePath(language, filename),
    na = '""',
    progress = FALSE
  )
}

splitTranslationFile <- function(slug, from.language, size.limit) {
  count <-  ceiling(file.size(filePath(from.language, slug)) / size.limit)
  if (count > 1) {
    df <- readFile(slug, from.language)
    seg.length <- floor(nrow(df) / count)
    dfs <- list()
    for (i in 1:count) {
      dfs[[i]] <- df[((i-1)*seg.length + 1):(i*seg.length),]
      writeToFile(dfs[[i]], paste(slug, i, sep =  " - "), from.language)
    }
  }
  return(count)
}
