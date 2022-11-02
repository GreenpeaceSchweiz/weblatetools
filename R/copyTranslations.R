#' Copy and edit translation files
#'
#' Download translation files for a Weblate project, edit them and then reupload them to the same or a different Weblate project.
#'
#' @param components A list of component names (slugs).
#' @param to.language The language to copy to.
#' @param from.project The project to copy from.
#' @param from.language The language to copy from.
#' @param conflict Optional. How to handle conflicts on the server. One of "ignore", "replace-translated" or "replace-approved".
#' @param verbose Optional. Whether to print a detailed log to the console or not.
#' @param download Optional. Whether to download translation files (uses files on disk if FALSE).
#' @param download.method Optional. Which method to use to download the translation files. See \code{\link{getFile}}.
#' @param upload Optional. Whether to upload translation files after download (and editing).
#' @param editFunction Optional. The function to edit the translation files. Defaults to \code{\link{editTranslationFile}}. Needs to have at least the parameters "slug" and "from.language", save processed files as "slug-i.csv" (with i in 1:n) and return n the number of created files. During upload, all of processed files will be uploaded.
#' @param ... Parameters to be passed to the editFunction. See \code{\link{editTranslationFile}}
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
           conflict = "ignore",
           verbose = FALSE,
           download = TRUE,
           download.method = "api",
           upload = TRUE,
           editFunction = editTranslationFile,
           ...) {
    outcomes <- data.frame(matrix(ncol = 8, nrow = 0))

    # Copy
    for (slug in components) {
      logger(verbose, slug)


      if (download) {
        # Get File
        response <- getFile(slug = slug,
                            from.project = from.project,
                            from.language = from.language,
                            method = download.method,
                            verbose = verbose)

        if (response$status_code != 200) next # couldn't get file, go to next
      }

      # Edit File (Filter, Replace, Split)
      chunks <- editFunction(slug, from.language, ...)


      if (upload) {
        # Post File
        responselist <- list()
        for (i in 1:chunks) {
          responselist[[i]] <- tryposting(
            slug = slug,
            to.language = to.language,
            from.directory = from.language,
            conflict = conflict,
            verbose = verbose,
            filename = processedFile(slug, i)
          )
        }

        # Store outcomes
        for (response in responselist) {
          outcomes <- rbind(outcomes, logEntry(slug, response))
        }
      }

    }

    if (upload) {
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
      logger(verbose, paste("DONE: Accepted ",
                            sum(outcomes$accepted),
                            " new translations into ",
                            wenv$TO_PROJECT,
                            "/",
                            to.language,
                            " from ",
                            from.project,
                            "/",
                            from.language))
      return(sum(outcomes$accepted))
    } else {
      return(0)
    }

  }

logEntry <- function(component, response) {
  if (response$status_code != 200) {
    result <- list(not_found = 0,
                   skipped = 0,
                   accepted = 0,
                   total = 0,
                   result = FALSE,
                   count = 0)
  } else {
    result <- httr::content(response)
  }
  return(cbind(component, cbind(response$status_code, as.data.frame(result))))
}

tryposting <- function(slug, to.language, from.directory, conflict, filename, verbose) {
  for (attempt in c(1:3)) {
    success <<- TRUE
    response <-
      tryCatch(
        expr = {
          postFile(slug = slug, to.language = to.language, from.directory = from.directory, conflict = conflict, filename = filename, verbose = verbose)
        },
        error = function(e){
          success <<- FALSE
          if (attempt == 1) logger(verbose, ">> writing -  HTTP error. Trying 3 times.")
          logger(verbose, paste(">> writing -  try #", attempt, sep = ""))
          print(e)
        },
        warning = function(w){
          print(w)
        },
        finally = {
        }
      )
    if (success) {
      return(response)
    }
  }
  if (!success) {
    response <- list(status_code = "failed")
    return(response)
  }
}
