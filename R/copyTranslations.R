#' Copy and edit translation files
#'
#' Download translation files for a Weblate project, edit them (search/replace &
#'  filter unwanted terms) and then reupload them to the same or a different
#'  Weblate project.
#'
#' @param components A list of component names (slugs).
#' @param to.language The language to copy to.
#' @param from.project The project to copy from.
#' @param from.language The language to copy from.
#' @param filter Optional. List of strings. Translations containing any of these strings will not be reuploaded.
#' @param replace Optional. Data frame of search/replace string pairs. Needs to contain a column "pattern" and a column "replace".
#' @param conflict Optional. How to handle conflicts on the server. One of "ignore", "replace-translated" or "replace-approved".
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#'
#' @return Count of accepted new translations in the destination project.
#' @export
#'
#' @examples
#' \dontrun{
#' updateCount <- copyTranslations(components = "address",
#'                                 to.language = "de_CH",
#'                                 from.project = "gpde",
#'                                 from.language = "de",
#'                                 filter = "Deutschland",
#'                                 replace = as.data.frame(cbind(pattern = c("ß"),
#'                                                               replace = c("ss"))))
#'}
copyTranslations <-
  function(components,
           to.language,
           from.project,
           from.language,
           filter,
           replace,
           conflict = "ignore",
           verbose = FALSE) {
    outcomes <- data.frame(matrix(ncol = 8, nrow = 0))

    # Copy
    for (slug in components) {
      if(verbose) cat(paste(slug, "\n"))
      response <- getFile(slug = slug, from.project = from.project, from.language = from.language, verbose = verbose)
      if (response$status_code == 200) {
        if (!missing(filter) || !missing(replace)) {
          editTranslationFile(slug, from.language, filter, replace)
        }
        response <-
          postFile(slug = slug, to.language = to.language, from.directory = from.language, conflict = conflict, verbose = verbose)
        if (response$status_code == 413) {
          splitTranslationFile(slug, from.language)
          response1 <-
            postFile(
              slug = slug,
              to.language = to.language,
              from.directory = from.language,
              conflict = conflict,
              verbose = verbose,
              filename = paste(slug, "- 1")
            )
          response2 <-
            postFile(
              slug = slug,
              to.language = to.language,
              from.directory = from.language,
              conflict = conflict,
              verbose = verbose,
              filename = paste(slug, "- 2")
            )
          responselist <- list(response1, response2)
        } else {
          responselist <- list(response)
        }
        for (response in responselist) {
          outcomes <- rbind(outcomes, logEntry(slug, response))
        }
      }
    }

    # Log Outcomes
    colnames(outcomes) <-
      c("component",
        "status code",
        "not found",
        "skipped",
        "accepted",
        "total",
        "result",
        "count")
    log_components <-
      rbind(c("Total", sum(outcomes$'status code' != 200), colSums(outcomes[,-c(1,2)])), outcomes)

    dir.create(file.path("logs"), showWarnings = FALSE)
    write.csv(
      log_components,
      paste(
        "logs/",
        format(Sys.time(), "%Y-%m-%d %H%M%S "),
        from.project,
        from.language,
        " outcomes.csv",
        sep = ""
      ),
      row.names = FALSE
    )
    if (file_test("-f", "log.csv")) {
      log_total <- read.csv("log.csv")
    } else {
      log_total <- data.frame(matrix(ncol = 9, nrow = 0))
    }

    log_total <- rbind(c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      paste(from.project, from.language),
      paste("gpch", to.language),
      colSums(outcomes[,-c(1,2)])
    ), log_total)
    colnames(log_total) <- c("timestamp","source","destination","not.found","skipped","accepted","total","result","count")
    write.csv(log_total, "log.csv", row.names = FALSE)
    if(verbose) cat(paste("DONE: Accepted ",
              sum(outcomes$accepted),
              " new translations into ",
              wenv$TO_PROJECT,
              "/",
              to.language,
              " from ",
              from.project,
              "/",
              from.language),
              "\n", sep = "")
    return(sum(outcomes$accepted))
  }

logEntry <- function(component, response) {
  result <- httr::content(response)
  if (response$status_code != 200) {
    result <- list(not_found = 0,
                   skipped = 0,
                   accepted = 0,
                   total = 0,
                   result = FALSE,
                   count = 0)
  }
  return(cbind(component, cbind(response$status_code, as.data.frame(result))))
}
