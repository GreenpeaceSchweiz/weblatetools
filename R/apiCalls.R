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

#' Download a translation file.
#'
#' Download a translation file from the location specified by a combination of
#' Project, Component, and Language.
#' The file is written to disk at "~/[from.language]/[slug].csv". Existing files
#' are overwritten.
#'
#' @param slug The slug of the component to download the translation file from.
#' @param from.project The project to download the file from.
#' @param from.language The language to download the file from.
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#'
#' @return The response from the server, as returned by httr::GET().
#' @export
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


#' Upload a translation file
#'
#' Upload the translation file stored at "~/[from.language]/[slug].csv" to the
#' specified language in the project defined in setup().
#'
#' @param slug The slug of the component to upload to.
#' @param to.language The language to upload to.
#' @param from.directory The directory the file is saved in.
#' @param conflict Optional. How to handle conflicts on the server. One of "ignore", "replace-translated" or "replace-approved".
#' @param filename Optional, defaults to [slug]. Name of the file, without ".csv" ending.
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#'
#' @return The response from the server, as returned by httr::POST().
#' @export
postFile <-  function(slug,
                      to.language,
                      from.directory,
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
                     from.directory, filename)
                   ),
                   conflict = conflict)
  )
  if(verbose) cat(paste(">> writing - ", httr::http_status(response)$message, "\n"))
  return(response)
}
