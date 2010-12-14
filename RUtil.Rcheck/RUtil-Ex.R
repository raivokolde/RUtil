pkgname <- "RUtil"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RUtil')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("make_unique_comb")
### * make_unique_comb

flush(stderr()); flush(stdout())

### Name: make_unique_comb
### Title: Function for producing all unique combinations of elements from
###   one vector.
### Aliases: make_unique_comb

### ** Examples
make_unique_comb(letters[1:3])


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
