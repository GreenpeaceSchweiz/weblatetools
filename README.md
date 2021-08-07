
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weblatetools

<!-- badges: start -->
<!-- badges: end -->

This package provides functions to (1) get a list of components for a
given Weblate project, and (2) download translation files for such a
list of components from a Weblate project, edit them (search/replace &
filter unwanted terms) and then reupload them to matching components in
the same or a different Weblate project.

## Installation

You can install weblatetools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sikaiser/weblatetools")
```

## Example

The following example shows how to copy translations for the language
“lang” from a project “A” to a project “B”, while filtering some
translations and replacing some strings.

``` r
library(weblatetools)

# copyTranslations() stores the translation files and creates log files.
# Choose an appropriate working directory. 
setwd("/myproject")

# Initialize some base parameters. "to.project" is set here instead of in 
# copyTranslations() to avoid mistakenly writing to the wrong project.
setup(api.url = "https://example.com/api",
      token = readLines("token.txt"),
      to.project = "B")

components <- getComponents("B")

updateCount <- weblatetools::copyTranslations(components = components$slugs,
                                to.language = "de",
                                from.project = "A",
                                from.language = "de",
                                filter = "filter this translation",
                                replace = as.data.frame(cbind(pattern = c("replace me"),
                                                              replace = c("with this"))),
                                verbose = TRUE)
```