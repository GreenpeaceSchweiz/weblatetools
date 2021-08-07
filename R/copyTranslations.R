copyTranslationsToGPCH <-
  function(to.components,
           to.language,
           from.country,
           from.language,
           filter,
           replace,
           conflict = "ignore") { # This is also the default server-side.
    outcomes <- data.frame(matrix(ncol = 8, nrow = 0))

    # Copy
    for (slug in to.components) {
      cat(slug)
      cat("\n")
      response <- getFile(slug, from.country, from.language)
      if (response$status_code == 200) {
        if (!missing(filter) || !missing(replace)) {
          editTranslationFile(slug, from.language, filter, replace)
        }
        response <-
          postFile(slug, to.language, from.language, conflict)
        if (response$status_code == 413) {
          splitTranslationFile(slug, from.language)
          response1 <-
            postFile(
              slug,
              to.language,
              from.language,
              conflict,
              filename = paste(slug, "- 1")
            )
          response2 <-
            postFile(
              slug,
              to.language,
              from.language,
              conflict,
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

    write.csv(
      log_components,
      paste(
        "logs/",
        format(Sys.time(), "%Y-%m-%d %H%M%S "),
        from.country,
        from.language,
        " outcomes.csv",
        sep = ""
      ),
      row.names = FALSE
    )
    log_total <- read.csv("log.csv")
    log_total <- rbind(c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      paste(from.country, from.language),
      paste("gpch", to.language),
      colSums(outcomes[,-c(1,2)])
    ), log_total)
    write.csv(log_total, "log.csv", row.names = FALSE)
    cat(paste("DONE: Accepted",
              sum(outcomes$accepted),
              "new translations into language",
              to.language,
              "from country",
              from.country))
    cat("\n")
  }

logEntry <- function(component, response) {
  result <- content(response)
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
