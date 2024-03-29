#' Get all components within a project.
#'
#' @param project The project you want to get the list of components from.
#'
#' @return A data frame with names and slugs for all components in the project.
#' @export
getComponents <- function(project) {
  componentsUrl <-
    paste(wenv$API_URL, "projects", project, "components", "", sep = "/") # last "" to create trailing "/"

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
#' @param method How to get the file. "api" or "public". If "public" is specified, the file is downloaded from the \code{public.url} provided in \code{\link{setup}}, without using the token for authorization. Only works if the file is publicly available.
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#'
#' @return The response from the server, as returned by httr::GET().
#' @export
getFile <- function(slug, from.project, from.language, method = "api", verbose = FALSE) {
  if (method == "api") {
    downloadUrl <- paste(
      wenv$API_URL,
      "translations",
      from.project,
      slug,
      from.language,
      "file",
      "?format=csv",
      sep = "/"
    )
    header <- c(Authorization = wenv$TOKEN)
  } else if (method == "public") {
    downloadUrl <- paste(
      wenv$BASE_URL,
      "download",
      from.project,
      slug,
      from.language,
      "?format=csv",
      sep = "/"
    )
    header <- ""
  } else {
    stop("Invalid method specified.")
  }

  response <- httr::GET(
    url = downloadUrl,
    config = httr::add_headers(header),
    httr::write_disk(filePath(from.language, slug), overwrite = TRUE) # TODO don't write to disk or make optional
  )

  logger(verbose, paste(">> getting - ", httr::http_status(response)$message))
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
  uploadUrl <- paste(wenv$API_URL,
                     "translations",
                     wenv$TO_PROJECT, # TODO: highlight importance of setting this right.
                     slug,
                     to.language,
                     "file",
                     "",
                     sep = "/")
  response <- httr::POST(url = uploadUrl,
                   httr::add_headers(Authorization = wenv$TOKEN),
                   httr::config(http_version = 2), # HTTP Version 1.1, to prevent "stream not closed cleanly"
                   body = list(file = httr::upload_file(filePath(
                     from.directory, filename)
                   ),
                   conflict = conflict,
                   fuzzy = "process")
  )
  logger(verbose, paste(">> writing - ", httr::http_status(response)$message))
  return(response)
}



#' Delete translations for a component on the server
#'
#' Deletes the translations of the specified component on the server.
#'
#' @param slug The component to delete from
#' @param project The project to delete form
#' @param language The language to delete from
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#'
#' @return The response from the server, as returned by httr::DELETE().
#' @export
deleteTranslations <- function(slug, project, language, verbose = FALSE) {
  deleteUrl <- paste(
    wenv$BASE_URL,
    "translations",
    project,
    slug,
    language,
    "",
    sep = "/"
  )
  response <- httr::DELETE(
    url = deleteUrl,
    config = httr::add_headers(Authorization = wenv$TOKEN)
  )

  logger(verbose, paste(">> deleting- ", httr::http_status(response)$message))
  return(response)
}

