library(roxygen)

setwd("~/Raivo/Projects/RUtil/")

roxygenize(package.dir = "Source", roxygen.dir = "RUtil", unlink.target = T, use.Rd2 = T)

system("R CMD check r_util")
