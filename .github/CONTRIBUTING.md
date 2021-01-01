# Contributing to dte-rate-comparison

This outlines how to propose a change to dte-rate-comparison.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue. 
If you've found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

- Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("ellisvalentiner/dte-rate-comparison", fork = TRUE)`.

- Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

- Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

- For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

- We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the  project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
