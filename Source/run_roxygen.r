library(roxygen)

setwd("~/Raivo/Projects/RUtil/")

roxygenize(package.dir = "Source", roxygen.dir = "RUtil", unlink.target = T, use.Rd2 = T)

## Run in terminal
cd ~/Raivo/Projects/RUtil/
R CMD check RUtil
R CMD build RUtil
R CMD install RUtil_0.04.tar.gz 


# Debug, kui kisab et dokument sisaldab mitte ascii tähti
tools::showNonASCII( readLines("RUtil/man/gprofiler.Rd"))