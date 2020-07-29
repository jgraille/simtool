# Clean up workspace
rm(list = ls(all.names = TRUE))
# Set environnement
if (!('renv' %in% installed.packages())) install.packages('renv')
library(renv)
# for the first run uncomment the row below
# renv::init() 
# Install the packages
required.packages = c("shiny","shinythemes","R6","rhandsontable","shinyWidgets","tidyverse")
new.packages = required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE, repos = "http://cran.us.r-project.org", lib=.libPaths()[1])
lapply(required.packages, function(x){
  print(x)
  require(x, character.only = TRUE)
})
#renv::snapshot()
rm(required.packages, new.packages)
