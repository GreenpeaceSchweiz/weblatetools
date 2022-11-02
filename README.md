
<!-- README.md is generated from README.Rmd. Please edit that file -->

# weblatetools

<!-- badges: start -->
<!-- badges: end -->

This package provides functions to:

1.  get a list of components for a given Weblate project
2.  download and upload translation files for components in a Weblate
    project
3.  bulk copy translation files for a list of components between Weblate
    projects, while editing them to search&replace and/or filter
    unwanted terms

## Installation

You can install weblatetools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GreenpeaceSchweiz/weblatetools")
```

## Example

The following example shows how to copy translations for the language
“lang” from a project “A” to a project “B”, while filtering some
translations and replacing some strings.

``` r
library(weblatetools)

# copyTranslations() stores the translation files and creates log files.
# Choose an appropriate working directory. 
setwd("~/myproject")

# Initialize some base parameters. "to.project" is set here instead of in 
# copyTranslations() or postFile() to avoid mistakenly writing to the wrong 
# project.
setup(api.url = "https://example.com",
      token = readLines("token.txt"),
      to.project = "B")

# Working with the components from the destination project is recommended.
# If, during the copy process, you try to get a component that does not exist in
# the source project, it will simply be skipped.
components <- getComponents("B")

copyTranslations(components = components$slugs,
                 to.language = "lang",
                 from.project = "A",
                 from.language = "lang",
                 filter = "filter this translation",
                 replace = as.data.frame(cbind(pattern = c("replace me"),
                                               replace = c("with this"))),
                 verbose = TRUE)
```
