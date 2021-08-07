getComponents <- function(country) {
  componentsUrl <-
    file.path(BASE_URL, "projects", country, "components")

  components <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(components) <- c("component", "slug")

  while (!is.null(componentsUrl)) {
    response <- GET(url = componentsUrl,
                    config = add_headers(Authorization = TOKEN))

    content <- content(response)

    componentsUrl <- content$'next'

    unlisted <- unlist(content)
    names <- unlisted[grepl("results.name", names(unlisted))]
    slugs <- unlisted[grepl("results.slug", names(unlisted))]
    components <- rbind(components, cbind(names, slugs))
  }
  rownames(components) <- c()
  return(components)
}

getFile <- function(slug, country, language) {
  downloadUrl <- paste(
    BASE_URL,
    "translations",
    country,
    slug,
    language,
    "file?format=csv",
    sep = "/"
  )
  response <- GET(
    url = downloadUrl,
    config = add_headers(Authorization = TOKEN),
    write_disk(filePath(language, slug), overwrite = TRUE)
  )

  cat(paste(">> getting", http_status(response)$message, sep = " - "))
  cat("\n")
  return(response)
}


postFile <-  function(slug,
                      to.language,
                      from.language,
                      conflict = "ignore",
                      filename) {
  if (missing(filename)) {
    filename <- slug
  }
  uploadUrl <- paste(BASE_URL,
                     "translations",
                     "gpch", # NEVER CHANGE. ONLY WRITE TO SWITZERLAND
                     slug,
                     to.language,
                     "file/",
                     sep = "/")
  response <- POST(url = uploadUrl,
                   config = add_headers(Authorization = TOKEN),
                   body = list(file = upload_file(filePath(
                     from.language, filename)
                   ),
                   conflict = conflict)
  )
  cat(paste(">> writing", http_status(response)$message, sep = " - "))
  cat("\n")
  return(response)
}
