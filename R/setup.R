wenv <- new.env(parent = emptyenv())


#' Initialize some basic environment variables needed for the API calls.
#'
#' @param base.url The URL of your Weblate instance. This is the same URL that you would use to access the Weblate web interface in your browser. "https://example.com" (no trailing "/").
#' @param token Your authorization token. See https://docs.weblate.org/en/latest/api.html#authentication-and-generic-parameters
#' @param to.project The project you want to upload the translation files to. CAREFUL: make sure to set this to the appropriate project. It is set here instead of in copyTranslations() or postFile() to avoid mistakenly writing to the wrong project.
#' @param debug Controls httr verbose logging (httr::verbose()). Defaults to FALSE
#'
#' @return
#' @export
setup <- function(base.url, token, to.project, debug = FALSE){
  wenv$BASE_URL <- base.url
  wenv$API_URL <- paste(base.url, "api", sep = "/")
  wenv$TOKEN <- paste("Token", token, sep = " ")
  wenv$TO_PROJECT <- to.project
  if(debug){
    httr::set_config(httr::verbose())
  } else {
    httr::reset_config()
  }
}

logger <- function(verbose, message) {
  if (verbose) cat(paste(message, "\n", sep = ""))
}
