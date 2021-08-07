#' Get all components within a project.
#'
#' @param project The project you want to get the list of components from.
#'
#' @return A data frame with names and slugs for all components in the project.
#' @export
getComponents <- function(project) {
  componentsUrl <-
    file.path(wenv$BASE_URL, "projects", project, "components")

  components <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(components) <- c("component", "slug")

  while (!is.null(componentsUrl)) {
    response <- httr::GET(url = componentsUrl,
                          config = httr::add_headers(Authorization = wenv$TOKEN))

    content <- httr::content(response)

    componentsUrl <- content$'next'

    unlisted <- unlist(content)
    names <- unlisted[grepl("results.name", names(unlisted))]
    slugs <- unlisted[grepl("results.slug", names(unlisted))]
    components <- rbind(components, cbind(names, slugs))
  }
  rownames(components) <- c()
  return(components)
}

getFile <- function(slug, from.project, from.language, verbose = FALSE) {
  downloadUrl <- paste(
    wenv$BASE_URL,
    "translations",
    from.project,
    slug,
    from.language,
    "file?format=csv",
    sep = "/"
  )
  response <- httr::GET(
    url = downloadUrl,
    config = httr::add_headers(Authorization = wenv$TOKEN),
    httr::write_disk(filePath(from.language, slug), overwrite = TRUE) # TODO don't write to disk or make optional
  )

  if(verbose) cat(paste(">> getting - ", httr::http_status(response)$message, "\n"))
  return(response)
}


postFile <-  function(slug,
                      to.language,
                      from.language,
                      conflict = "ignore",
                      filename,
                      verbose = FALSE) {
  if (missing(filename)) {
    filename <- slug
  }
  uploadUrl <- paste(wenv$BASE_URL,
                     "translations",
                     wenv$TO_PROJECT, # TODO: highlight importance of setting this right.
                     slug,
                     to.language,
                     "file/",
                     sep = "/")
  response <- httr::POST(url = uploadUrl,
                   config = httr::add_headers(Authorization = wenv$TOKEN),
                   body = list(file = httr::upload_file(filePath(
                     from.language, filename)
                   ),
                   conflict = conflict)
  )
  if(verbose) cat(paste(">> writing - ", httr::http_status(response)$message, "\n"))
  return(response)
}
