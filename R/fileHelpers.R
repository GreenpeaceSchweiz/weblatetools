filePath <- function(language, fileName) {
  dir.create(file.path(language), showWarnings = FALSE)
  path <- file.path(language, paste(fileName, "csv", sep = "."))
  return(path)
}

processedFile <- function(slug, i = 1) {
  return(paste(slug, i, sep = " - "))
}

readFile <- function(filename, language) {
  df <- readr::read_csv(filePath(language, filename),
                        col_types = "cccccccc",
                        lazy = FALSE) # to prevent locking of the file
  return(df)
}

writeToFile <- function(df, filename, language) {
  readr::write_csv(
    df,
    filePath(language, filename),
    na = '""',
    quote = "all",
    progress = FALSE
  )
}


