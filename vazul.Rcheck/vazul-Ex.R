pkgname <- "vazul"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('vazul')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("marp")
### * marp

flush(stderr()); flush(stdout())

### Name: marp
### Title: MARP: Many Analysts Religion Project dataset
### Aliases: marp
### Keywords: cross-cultural datasets religion well-being

### ** Examples

## Not run: 
##D library(dplyr)
##D data(marp)
##D ## quick checks
##D dim(marp)
##D glimpse(marp)
##D ## country-level mean religiosity
##D marp  |>
##D   select(country, starts_with("rel_")) |>
##D   group_by(country) |>
##D   summarise(across(starts_with("rel_"), ~ mean(.x, na.rm = TRUE)))
## End(Not run)



cleanEx()
nameEx("williams")
### * williams

flush(stderr()); flush(stdout())

### Name: williams
### Title: Stereotyping of High-Wealth Individuals in Different Ecologies
### Aliases: williams
### Keywords: datasets

### ** Examples

data(williams)
str(williams)
table(williams$ecology)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
