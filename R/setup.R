wenv <- new.env(parent = emptyenv())


#' Initialize some basic environment variables needed for the API calls.
#'
#' @param api.url The URL of your API entry point. Usually ""http://example.com/api/".
#' @param token Your authorization token. See https://docs.weblate.org/en/latest/api.html#authentication-and-generic-parameters
#' @param to.project The project you want to upload the translation files to. CAREFUL: make sure to set this to the appropriate project. It is set here instead of in copyTranslations() or postFile() to avoid mistakenly writing to the wrong project.
#' @param debug Controls httr verbose logging (httr::verbose()). Defaults to FALSE
#'
#' @return
#' @export
setup <- function(api.url, token, to.project, debug = FALSE){
  wenv$BASE_URL <- api.url
  wenv$TOKEN <- paste("Token", token, sep = " ")
  wenv$TO_PROJECT <- to.project
  if(debug){
    httr::set_config(httr::verbose())
  }
}

logger <- function(verbose, message) {
  if (verbose) cat(paste(message, "\n", sep = ""))
}
