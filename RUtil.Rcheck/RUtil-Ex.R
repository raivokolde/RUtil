pkgname <- "RUtil"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RUtil')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("gconvert")
### * gconvert

flush(stderr()); flush(stdout())

### Name: gconvert
### Title: Convert gene ID-s.
### Aliases: gconvert

### ** Examples
gconvert(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")


cleanEx()
nameEx("generate_gcocoa_query")
### * generate_gcocoa_query

flush(stderr()); flush(stdout())

### Name: generate_gcocoa_query
### Title: Generate query for g:Cocoa web tool...
### Aliases: generate_gcocoa_query

### ** Examples
glist = list(a = c("pax6", "klf9"), b = c("nanog", "Pou5f1"))


cleanEx()
nameEx("gorth")
### * gorth

flush(stderr()); flush(stdout())

### Name: gorth
### Title: Find orthologs.
### Aliases: gorth

### ** Examples
gorth(c("Klf4", "Pax5", "Sox2", "Nanog"), source_organism = "mmusculus", target_organism = "hsapiens")


cleanEx()
nameEx("gprofiler")
### * gprofiler

flush(stderr()); flush(stdout())

### Name: gprofiler
### Title: Annotate gene list functionally.
### Aliases: gprofiler

### ** Examples
gprofiler(c("Klf4", "Pax5", "Sox2", "Nanog"), organism = "mmusculus")


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
