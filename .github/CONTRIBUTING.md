# Contributing to specieshindex

This outlines how to propose a change to specieshindex.

## Fixing typos
Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface, so long as the changes are made in the source file. To fix any typos, please edit the related `.R` file in the `R/` folder, instead of the `.Rd` file in the `man/` folder.

## Fixing bugs
If you have found a bug in the code, please create an associated issue with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex) to properly illustrate the bug. You could also propose new functionality by creating an issue for the wider community to discuss and implement. 

## Pull request process
- We recommend that you create a Git branch for each pull request (PR).
- Look at the Travis and AppVeyor build status before and after making changes. The `README` should contain badges for any continuous integration services used by the package.
- We use [roxygen2](https://cran.r-project.org/web/packages/roxygen2/index.html), with [Markdown syntax](https://bookdown.org/yihui/rmarkdown/), for documentation.
- We use [testthat](https://cran.r-project.org/web/packages/testthat/index.html). Contributions with test cases included are easier to accept.
- For user-facing changes, add a bullet to the top of `NEWS.md` below the current development version header describing the changes made followed by your GitHub username, and links to relevant issue(s)/PR(s).
