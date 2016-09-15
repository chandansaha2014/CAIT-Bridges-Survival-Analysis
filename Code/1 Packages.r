###########################################################################
#  Title: CAIT Bridge Survival Analysis
#   Date: Sep 2016
# Author: Chandan Saha
#
#
# Description:
#
# This script points the tool to the local package repository, loads all
# required packages, and downloads them to the local repository if needed.
# All packages needed for the tool should be listed here and should not
# be called via 'library' anywhere else in the tool.
#
###########################################################################


### Begin loading tool components message
cat("Loading tool components")


# 
# Function to auto-load packages (will attempt to download if not found)
LoadPackages <- function(packages) {

  notinstalled <- packages[!packages %in% .packages(all.available = TRUE)]
  if (length(notinstalled) > 0) install.packages(notinstalled, repos = "http://cran.r-project.org", dependencies = TRUE)
  for (package in packages) {cat("."); eval(parse(text = paste0("suppressMessages(library(", package, "))")))}

}

# Load necessary packages 
packages <- c("Amelia","VIM" ,"flexsurv","survival","ggplot2","rms")
LoadPackages(packages)
