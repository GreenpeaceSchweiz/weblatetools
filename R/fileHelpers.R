filePath <- function(language, fileName) {
  dir.create(file.path(language), showWarnings = FALSE)
  path <- file.path(language, paste(fileName, "csv", sep = "."))
  return(path)
}

readFile <- function(filename, language) {
  df <- read.csv(filePath(language, filename), fileEncoding = "UTF-8")
  return(df)
}

writeToFile <- function(df, filename, language) {
  write.csv(
    df,
    filePath(language, filename),
    na = '""',
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

splitTranslationFile <- function(slug, from.language) {
  df <- readFile(slug, from.language)
  midpoint <- floor((nrow(df)/2))
  df1 <- df[1:midpoint,]
  df2 <- df[(midpoint+1):nrow(df),]
  writeToFile(df1, paste(slug, "- 1"), from.language)
  writeToFile(df2, paste(slug, "- 2"), from.language)
}
